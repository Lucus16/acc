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
   many, notFollowedBy, runParserT, satisfy, some, takeWhile1P, takeWhileP, try, unPos)
import Text.Megaparsec.Char.Lexer (decimal)
import Data.Char (isAlpha, isControl, isDigit, isSpace)

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

character :: Parser Expression
character = sourced $ fmap Character $ do
  void $ chunk "'"
  c <- satisfy (not . needsEscape) <|> escapeSequence
  void $ chunk "'"
  space
  pure c
  where
    needsEscape c = isControl c || c == '\\' || c == '\''
    escapeSequence = chunk "\\" >>
      (   '\\' <$ chunk "\\"
      <|> '\"' <$ chunk "\""
      <|> '\'' <$ chunk "'"
      <|> '\0' <$ chunk "0"
      <|> '\a' <$ chunk "a"
      <|> '\n' <$ chunk "n"
      <|> '\r' <$ chunk "r"
      <|> '\t' <$ chunk "t")

arguments :: Parser [Expression]
arguments = parenthesized $ separateBy comma expression

tuple :: Parser Expression
tuple = sourced $ Builtin Tuple <$> arguments

literal :: Parser Expression
literal = integer <|> boolean <|> character <|> tuple <|> emptyList

variable :: Parser Expression
variable = sourced $ Variable <$> identifier

term :: Parser Expression
term = literal <|> variable

sourcedAccess :: Expression -> Sourced Name -> Expression
sourcedAccess lhs (Sourced rhsSrc rhs) = Sourced (rhsSrc <> source lhs) $ Access lhs rhs

trailingAccess :: Parser (Expression -> Expression)
trailingAccess = fmap (flip sourcedAccess) $ sourced $ chunk "." >> identifier

sourcedCall :: Expression -> Sourced [Expression] -> Expression
sourcedCall lhs (Sourced rhsSrc rhs) = Sourced (rhsSrc <> source lhs) $ Call lhs rhs

trailingCall :: Parser (Expression -> Expression)
trailingCall = flip sourcedCall <$> sourced arguments

postfix :: Parser Expression
postfix = foldl (&) <$> term <*> many rest
  where rest = trailingAccess <|> trailingCall

negation :: Parser Expression
negation = sourced $ fmap (Builtin Negate . pure) $ symbol "-" *> prefix

inversion :: Parser Expression
inversion = sourced $ fmap (Builtin Not . pure) $ symbol "!" *> prefix

prefix :: Parser Expression
prefix = negation <|> inversion <|> postfix

emptyList :: Parser Expression
emptyList = sourced $ bracketed $ pure $ Builtin EmptyList []

comma :: Parser ()
comma = symbol ","

bracketed :: Parser a -> Parser a
bracketed p = symbol "[" *> p <* symbol "]"

braced :: Parser a -> Parser a
braced p = symbol "{" *> p <* symbol "}"

parenthesized :: Parser a -> Parser a
parenthesized p = symbol "(" *> p <* symbol ")"

separateBy :: Parser a -> Parser b -> Parser [b]
separateBy sep p = ((:) <$> p <*> many (sep >> p)) <|> pure []

separateSomeBy :: Parser a -> Parser b -> Parser [b]
separateSomeBy sep p = (:) <$> p <*> many (sep >> p)

separateMultipleBy :: Parser a -> Parser b -> Parser [b]
separateMultipleBy sep p = (:) <$> p <*> some (sep >> p)

sourcedBuiltin :: Builtin -> Expression -> Expression -> Expression
sourcedBuiltin op lhs rhs = Sourced (source lhs <> source rhs) $ Builtin op [lhs, rhs]

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
multiplicative = binary prefix many [Multiply, Divide, Modulo]

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
  stmts <- many statement
  symbol "}"
  pure $ Fundec name args typ stmts

tupleType :: Parser (Type Name)
tupleType = parenthesized $ TupleOf <$> separateBy comma valueType

valueType :: Parser (Type Name)
valueType =
  (keyword "Bool" $> Bool)
  <|> (keyword "Char" $> Char)
  <|> (keyword "Int" $> Int)
  <|> tupleType
  <|> (ListOf <$> bracketed valueType)
  <|> (Typevar <$> identifier)

functionType :: Parser (Type Name)
functionType = do
  params <- many valueType
  symbol "->"
  ret <- (keyword "Void" $> Void) <|> valueType
  pure $ FunctionOf params ret

returnStatement :: Parser (Statement Name)
returnStatement = fmap Return $ keyword "return" *> optional expression <* symbol ";"

ifStatement :: Parser (Statement Name)
ifStatement = do
  c <- keyword "if" >> parenthesized expression
  t <- braced $ many statement
  f <- fromMaybe [] <$> optional (keyword "else" >> braced (many statement))
  pure $ If c t f

whileStatement :: Parser (Statement Name)
whileStatement = While <$> (keyword "while" >> parenthesized expression) <*> many statement

call :: Parser Expression
call = sourced $ Call <$> variable <*> arguments

callStatement :: Parser (Statement Name)
callStatement = Expression <$> call <* symbol ";"

lValue :: Parser (Expression -> Statement Name)
lValue = Assign <$> identifier <*> many (chunk "." >> identifier)

assignment :: Parser (Statement Name)
assignment = lValue <* symbol "=" <*> expression <* symbol ";"

comment :: Parser (Statement Name)
comment = fmap (Comment . Text.pack) $ chunk "#" *> many (satisfy (/='\n')) <* space

statement :: Parser (SourcedStatement Name)
statement = sourced $
  comment
  <|> returnStatement
  <|> ifStatement
  <|> whileStatement
  <|> try callStatement
  <|> try assignment
  <|> declaration

declaration :: Parser (Statement Name)
declaration = try vardec <|> fundec

file :: Parser [Sourced (Statement Name)]
file = many $ sourced declaration

parse :: Parser a -> String -> Text -> Either (ParseErrorBundle Text Void) a
parse parser path input = evalState
  (runParserT parser path input)
  (error "no end position available before parsing anything")
