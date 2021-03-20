module SPL.Parser where

import Control.Applicative (Alternative, empty)
import Data.Foldable (asum)
import Data.Function ((&))
import Data.Functor (($>), void)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Void (Void)
import Control.Monad.State (State, get, put, evalState)
import Text.Megaparsec
  ((<|>), ParseErrorBundle, ParsecT, SourcePos(..), Stream, Token, Tokens, chunk, getSourcePos,
   many, notFollowedBy, runParserT, satisfy, some, takeWhile1P, takeWhileP, unPos)
import Text.Megaparsec.Char.Lexer (decimal)
import Data.Char (isAlpha, isDigit, isSpace)

import SPL.Syntax

type Parser = ParsecT Void Text (State SourcePos)
type Name = Text
type Expression = SourcedExpr Name

identChar :: Char -> Bool
identChar c = isAlpha c || isDigit c || c == '_'

identFirstChar :: Char -> Bool
identFirstChar = isAlpha

optional :: (Alternative f, Alternative g) => f a -> f (g a)
optional x = fmap pure x <|> pure empty

manyP :: (Stream s, Ord e) => (Token s -> Bool) -> ParsecT e s m (Tokens s)
manyP = takeWhileP Nothing

someP :: (Stream s, Ord e) => (Token s -> Bool) -> ParsecT e s m (Tokens s)
someP = takeWhile1P Nothing

space :: Parser ()
space = void $ getSourcePos >>= put >> manyP isSpace

lexeme :: Parser a -> Parser a
lexeme p = p <* space

symbol :: Text -> Parser ()
symbol = void . lexeme . chunk

keyword :: Text -> Parser Text
keyword k = chunk k <* notFollowedBy (satisfy identChar) <* space

sourced :: Parser a -> Parser (Sourced a)
sourced p = do
  SourcePos path startLine startCol <- getSourcePos
  unSourced <- p
  SourcePos _path endLine endCol <- get
  let source = Source path (unPos startLine) (unPos startCol) (unPos endLine) (unPos endCol)
  pure Sourced { source, unSourced }

identifier :: Parser Name
identifier = lexeme $ Text.cons <$> satisfy identFirstChar <*> manyP identChar

integer :: Parser Expression
integer = sourced $ lexeme $ Integer <$> decimal

boolean :: Parser Expression
boolean = sourced $ fmap Boolean $ (keyword "False" $> False) <|> (keyword "True" $> True)

literal :: Parser Expression
literal = integer <|> boolean

variable :: Parser Expression
variable = sourced $ Variable <$> identifier

call :: Parser Expression
call = sourced $ Call <$> identifier <*> parenthesized (separateBy comma expression)

negation :: Parser Expression
negation = sourced $ fmap (Builtin Negate . pure) $ symbol "-" *> term

inversion :: Parser Expression
inversion = sourced $ fmap (Builtin Not . pure) $ symbol "!" *> term

term :: Parser Expression
term = literal <|> variable <|> call <|> negation <|> inversion <|> parenthesized expression

comma :: Parser ()
comma = symbol ","

parenthesized :: Parser a -> Parser a
parenthesized p = symbol "(" *> p <* symbol ")"

separateBy :: Parser a -> Parser b -> Parser [b]
separateBy sep p = ((:) <$> p <*> many (sep >> p)) <|> pure []

separateSomeBy :: Parser a -> Parser b -> Parser [b]
separateSomeBy sep p = (:) <$> p <*> many (sep >> p)

separateMultipleBy :: Parser a -> Parser b -> Parser [b]
separateMultipleBy sep p = (:) <$> p <*> some (sep >> p)

operatorText :: Builtin -> Text
operatorText Add = "+"
operatorText Subtract = "-"
operatorText Multiply = "*"
operatorText Divide = "/"
operatorText Modulo = "%"
operatorText Equal = "=="
operatorText NotEqual = "!="
operatorText LessThan = "<"
operatorText LessOrEqual = "<="
operatorText GreaterThan = ">"
operatorText GreaterOrEqual = ">="
operatorText And = "&&"
operatorText Or = "||"
operatorText Prepend = ":"
operatorText x = error $ "not an operator: " <> show x

sourcedBuiltin :: Builtin -> Expression -> Expression -> Expression
sourcedBuiltin op lhs rhs = Sourced src $ Builtin op [lhs, rhs]
  where
    Source path startLine startCol _ _ = source lhs
    Source _ _ _ endLine endCol = source rhs
    src = Source path startLine startCol endLine endCol

trailingBinary :: Parser Expression -> Builtin -> Parser (Expression -> Expression)
trailingBinary sub op = symbol (operatorText op) >> fmap (flip $ sourcedBuiltin op) sub

binary
  :: Parser Expression
  -> (forall a. Parser a -> Parser [a])
  -> [Builtin]
  -> Parser Expression
binary sub count ops = foldl (&) <$> sub <*> count rest
  where rest = asum $ map (trailingBinary sub) ops

multiplicative :: Parser Expression
multiplicative = binary term many [Multiply, Divide, Modulo]

additive :: Parser Expression
additive = binary multiplicative many [Add, Subtract]

relational :: Parser Expression
relational = binary additive optional [LessOrEqual, LessThan, GreaterOrEqual, GreaterThan]

equality :: Parser Expression
equality = binary relational optional [NotEqual, Equal]

logicalAnd :: Parser Expression
logicalAnd = binary equality many [And]

logicalOr :: Parser Expression
logicalOr = binary logicalAnd many [Or]

expression :: Parser Expression
expression = logicalOr

vardec :: Parser (Statement Name)
vardec = do
  typ <- (Nothing <$ keyword "var") <|> (Just <$> valueType)
  name <- identifier
  symbol "="
  value <- expression
  symbol ";"
  pure $ Vardec typ name value

fundec :: Parser (Statement Name)
fundec = do
  name <- identifier
  args <- parenthesized $ separateBy comma identifier
  typ <- optional $ symbol "::" >> functionType
  symbol "{"
  stmts <- many $ sourced statement
  symbol "}"
  pure $ Fundec name args typ stmts

valueType :: Parser (Type Name)
valueType =
  (keyword "Bool" $> Bool)
  <|> (keyword "Char" $> Char)
  <|> (keyword "Int" $> Int)
  <|> parenthesized (TupleOf <$> separateMultipleBy comma valueType)
  <|> (symbol "[" *> fmap ListOf valueType <* symbol "]")

functionType :: Parser (Type Name)
functionType = do
  params <- many valueType
  symbol "->"
  ret <- (keyword "Void" $> Void) <|> valueType
  pure $ FunctionOf params ret

returnStatement :: Parser (Statement Name)
returnStatement = fmap Return $ keyword "return" *> expression <* symbol ";"

ifStatement :: Parser (Statement Name)
ifStatement = do
  c <- keyword "if" >> parenthesized expression
  t <- many statement
  f <- fromMaybe [] <$> optional (keyword "else" >> many statement)
  pure $ If c t f

whileStatement :: Parser (Statement Name)
whileStatement = While <$> (keyword "while" >> parenthesized expression) <*> many statement

callStatement :: Parser (Statement Name)
callStatement = Expression <$> call <* symbol ";"

statement :: Parser (Statement Name)
statement = returnStatement <|> ifStatement <|> whileStatement <|> callStatement

file :: Parser [Statement Name]
file = many statement

parse :: Parser a -> String -> Text -> Either (ParseErrorBundle Text Void) a
parse parser path input = evalState (error "no end position available before parsing anything") $
  runParserT parser path input
