module Parser where

import Prelude hiding (init, product, sum)

import Control.Applicative (optional, (<|>))
import Data.Char (isAlpha, isDigit, isSpace)
import Data.Foldable (asum)
import Data.Function ((&))
import Data.Functor (void, ($>))
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Void (Void)
import Text.Megaparsec
  (Parsec, ParsecT, Stream, Token, Tokens, between, chunk, eof, many,
  notFollowedBy, satisfy, sepBy, takeWhile1P, takeWhileP, try)
import Text.Megaparsec.Char.Lexer qualified as L (decimal, lexeme, symbol)

import C
import Types (Literal(..), Type(..))

type Parser = Parsec Void Text

identChar :: Char -> Bool
identChar c = isAlpha c || isDigit c || c == '_'

identFirstChar :: Char -> Bool
identFirstChar c = isAlpha c || c == '_'

manyP :: (Stream s, Ord e) => (Token s -> Bool) -> ParsecT e s m (Tokens s)
manyP = takeWhileP Nothing

someP :: (Stream s, Ord e) => (Token s -> Bool) -> ParsecT e s m (Tokens s)
someP = takeWhile1P Nothing

space :: Parser ()
space = void $ manyP isSpace

lexeme :: Parser a -> Parser a
lexeme = L.lexeme space

symbol :: Text -> Parser ()
symbol = void . L.symbol space

keyword :: Text -> Parser Text
keyword k = chunk k <* notFollowedBy (satisfy identChar) <* space

identifier :: Parser Identifier
identifier = lexeme $ Text.cons <$> satisfy identFirstChar <*> manyP identChar

integer :: Parser Literal
integer = Integer <$> lexeme L.decimal

literal :: Parser Literal
literal = integer

term :: Parser Expression
term = parenthesized expression
  <|> Literal <$> literal
  <|> Variable <$> identifier

braced :: Parser a -> Parser a
braced = between (symbol "{") (symbol "}")

parenthesized :: Parser a -> Parser a
parenthesized = between (symbol "(") (symbol ")")

binaryText :: Binary -> Text
binaryText Add = "+"
binaryText Sub = "-"
binaryText Mul = "*"
binaryText Div = "/"
binaryText Mod = "%"
binaryText Eq  = "=="
binaryText Neq = "!="
binaryText Lt  = "<"
binaryText Leq = "<="
binaryText Gt  = ">"
binaryText Geq = ">="
binaryText And = "&&"
binaryText Or  = "||"
binaryText Shl = "<<"
binaryText Shr = ">>"
binaryText Com = ","

trailingBinary :: Parser Expression -> Binary -> Parser (Expression -> Expression)
trailingBinary sub op = symbol (binaryText op) *> fmap (flip $ Binary op) sub

binarySequence :: Parser Expression -> [Binary] -> Parser Expression
binarySequence sub ops = foldl (&) <$> sub <*> many rest
  where rest = asum $ map (trailingBinary sub) ops

multiplicative :: Parser Expression
multiplicative = binarySequence unary [Mul, Div, Mod]

additive :: Parser Expression
additive = binarySequence multiplicative [Add, Sub]

shift :: Parser Expression
shift = binarySequence additive [Shl, Shr]

relational :: Parser Expression
relational = binarySequence shift [Leq, Lt, Geq, Gt]

equality :: Parser Expression
equality = binarySequence relational [Eq, Neq]

logicalAnd :: Parser Expression
logicalAnd = binarySequence equality [And]

logicalOr :: Parser Expression
logicalOr = binarySequence logicalAnd [Or]

conditional :: Parser Expression
conditional = foldr ($) <$> logicalOr <*> many rest
  where
    rest :: Parser (Expression -> Expression)
    rest = do
      symbol "?"
      t <- expression
      symbol ":"
      f <- conditional
      pure $ \c -> Ternary c t f

assignment :: Parser Expression
assignment = try (Assignment <$> identifier <* symbol "=" <*> assignment) <|> conditional

expression :: Parser Expression
expression = binarySequence assignment [Com]

arguments :: Parser [Expression]
arguments = assignment `sepBy` symbol ","

postfix :: Parser (Expression -> Expression)
postfix = flip Call <$> parenthesized arguments

postfixExpression :: Parser Expression
postfixExpression = foldl (&) <$> term <*> many postfix

unary :: Parser Expression
unary = Unary Neg <$> (symbol "-" *> unary)
  <|> Unary Inv <$> (symbol "~" *> unary)
  <|> Unary Not <$> (symbol "!" *> unary)
  <|> postfixExpression

nullStatement :: Parser Statement
nullStatement = symbol ";" >> pure Inert

breakStatement :: Parser Statement
breakStatement = keyword "break" >> symbol ";" >> pure Break

continueStatement :: Parser Statement
continueStatement = keyword "continue" >> symbol ";" >> pure Continue

returnStatement :: Parser Statement
returnStatement = fmap Return $ keyword "return" *> expression <* symbol ";"

declaration :: Parser Statement
declaration = Declaration <$> type_ <*> identifier <*> optional (symbol "=" *> expression) <* symbol ";"

exprStatement :: Parser Statement
exprStatement = Expression <$> expression <* symbol ";"

ifStatement :: Parser Statement
ifStatement = do
  c <- keyword "if" >> parenthesized expression
  t <- statement
  f <- optional (keyword "else" >> statement)
  pure $ If c t f

whileStatement :: Parser Statement
whileStatement = While <$> (keyword "while" >> parenthesized expression) <*> statement

doWhileStatement :: Parser Statement
doWhileStatement = do
  body <- keyword "do" >> statement
  condition <- keyword "while" >> parenthesized expression
  symbol ";"
  pure $ DoWhile body condition

forStatement :: Parser Statement
forStatement = do
  _ <- keyword "for"
  symbol "("
  init <- nullStatement <|> declaration <|> exprStatement
  cond <- fromMaybe (Literal $ Integer 1) <$> optional expression
  symbol ";"
  step <- maybe Inert Expression <$> optional expression
  symbol ")"
  For init cond step <$> statementOrDeclaration

statementOrDeclaration :: Parser Statement
statementOrDeclaration = try statement <|> declaration

statement :: Parser Statement
statement = block
  <|> breakStatement
  <|> continueStatement
  <|> doWhileStatement
  <|> forStatement
  <|> ifStatement
  <|> returnStatement
  <|> whileStatement
  <|> nullStatement
  <|> exprStatement

block :: Parser Statement
block = fmap Block $ braced $ many statementOrDeclaration

type_ :: Parser Type
type_ = keyword "int" $> Int
    <|> keyword "unsigned" <> keyword "int" $> Word

parameter :: Parser (Parameter Type Identifier)
parameter = Parameter <$> type_ <*> identifier

parameters :: Parser [Parameter Type Identifier]
parameters = parenthesized $ parameter `sepBy` symbol ","

globalDeclaration :: Type -> Identifier -> Parser TopLevel
globalDeclaration typ name = do
  symbol ";"
  pure $ GlobalDeclaration typ name

globalDefinition :: Type -> Identifier -> Parser TopLevel
globalDefinition typ name = do
  symbol "="
  value <- conditional
  symbol ";"
  pure $ GlobalDefinition typ name value

function :: Type -> Identifier -> Parser TopLevel
function returnType name = do
  params <- parameters
  (FunctionDeclaration returnType name params <$ symbol ";")
    <|> (FunctionDefinition returnType name params <$> braced (many statementOrDeclaration))

topLevel :: Parser TopLevel
topLevel = do
  typ <- type_
  name <- identifier
  function typ name <|> globalDeclaration typ name <|> globalDefinition typ name

file :: Parser [TopLevel]
file = space *> many topLevel <* eof
