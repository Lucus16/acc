{-# LANGUAGE OverloadedStrings #-}

module Parser where

import Prelude hiding (product, sum)

import Control.Applicative (optional, (<|>))
import Data.Char (isAlpha, isDigit, isSpace)
import Data.Foldable (asum)
import Data.Function ((&))
import Data.Functor (void)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Void (Void)
import Text.Megaparsec
  (Parsec, ParsecT, Stream, Token, Tokens, between, chunk, eof, many, notFollowedBy,
  satisfy, takeWhile1P, takeWhileP, try)
import qualified Text.Megaparsec.Char.Lexer as L (decimal, lexeme, symbol)

import C
import qualified Expr

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

symbol :: Text -> Parser Text
symbol = L.symbol space

keyword :: Text -> Parser Text
keyword k = chunk k <* notFollowedBy (satisfy identChar) <* space

identifier :: Parser Identifier
identifier = lexeme $ Text.cons <$> satisfy identFirstChar <*> manyP identChar

integer :: Parser Literal
integer = Integer <$> lexeme L.decimal

literal :: Parser Literal
literal = integer

term :: Parser Term
term = Literal <$> literal <|> Variable <$> identifier

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
      _ <- symbol "?"
      t <- expression
      _ <- symbol ":"
      f <- conditional
      pure $ \c -> Ternary c t f

assignment :: Parser Expression
assignment = try (Assignment <$> identifier <* symbol "=" <*> assignment) <|> conditional

expression :: Parser Expression
expression = binarySequence assignment [Com]

unary :: Parser Expression
unary = Expr.Term <$> term
  <|> Unary Neg <$> (symbol "-" *> unary)
  <|> Unary Inv <$> (symbol "~" *> unary)
  <|> Unary Not <$> (symbol "!" *> unary)
  <|> parenthesized expression

nullStatement :: Parser Statement
nullStatement = keyword ";" >> pure Inert

returnStatement :: Parser Statement
returnStatement = fmap Return $ keyword "return" *> expression <* symbol ";"

declaration :: Parser Statement
declaration = Declaration <$> (keyword "int" *> identifier) <*> optional (symbol "=" *> expression) <* symbol ";"

exprStatement :: Parser Statement
exprStatement = Expression <$> expression <* symbol ";"

ifStatement :: Parser Statement
ifStatement = do
  c <- symbol "if" >> parenthesized expression
  t <- branch
  f <- optional (symbol "else" >> branch)
  pure $ If c t f

statement :: Parser Statement
statement = block <|> ifStatement <|> returnStatement <|> nullStatement <|> declaration <|> exprStatement

block :: Parser Statement
block = fmap Block $ between (symbol "{") (symbol "}") $ many statement

branch :: Parser Statement
branch = block <|> ifStatement <|> returnStatement <|> exprStatement

type_ :: Parser Type
type_ = keyword "int"

parameters :: Parser Parameters
parameters = parenthesized $ pure []

topLevel :: Parser TopLevel
topLevel = Fdef <$> type_ <*> identifier <*> parameters <*> block

file :: Parser [TopLevel]
file = space *> many topLevel <* eof
