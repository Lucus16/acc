{-# LANGUAGE OverloadedStrings #-}

module C where

import Control.Applicative ((<|>))
import Data.Char (isAlpha, isDigit, isSpace)
import Data.Functor (void)
import Data.Text (Text)
import Data.Void (Void)
import Text.Megaparsec (Parsec, ParsecT, Stream, Token, Tokens, between, many, satisfy, takeWhileP, takeWhile1P)
import qualified Text.Megaparsec.Char.Lexer as L (decimal, lexeme, symbol)
import qualified Data.Text as Text

type Parser = Parsec Void Text

type AssemblyLines = [Text]

type Identifier = Text

type Type = Identifier

type Body = [Statement]

type Parameter = ()

type Parameters = [Parameter]

data FunctionDefinition
  = Fdef Type Identifier Parameters Body
  deriving (Show)

data Statement
  = Return Expression
  deriving (Show)

data Expression
  = Term Term
  | Negate Expression
  | Invert Expression
  | Not Expression
  deriving (Show)

data Term
  = Literal Literal
  deriving (Show)

data Literal
  = Integer Integer
  deriving (Show)

tshow :: Show a => a -> Text
tshow = Text.pack . show

identChar :: Char -> Bool
identChar c = isAlpha c || isDigit c || c == '_'

identFirstChar :: Char -> Bool
identFirstChar c = isAlpha c || c == '_'

manyP :: (Stream s, Ord e) => (Token s -> Bool) -> ParsecT e s m (Tokens s)
manyP = takeWhileP Nothing

someP :: (Stream s, Ord e) => (Token s -> Bool) -> ParsecT e s m (Tokens s)
someP = takeWhile1P Nothing

identifier :: Parser Identifier
identifier = Text.cons <$> satisfy identFirstChar <*> manyP identChar

space :: Parser ()
space = void $ manyP isSpace

lexeme :: Parser a -> Parser a
lexeme = L.lexeme space

symbol :: Text -> Parser Text
symbol = L.symbol space

integer :: Parser Literal
integer = Integer <$> L.decimal

literal :: Parser Literal
literal = integer

term :: Parser Term
term = Literal <$> literal

expression :: Parser Expression
expression = Term <$> term
  <|> Negate <$> (symbol "-" *> expression)
  <|> Invert <$> (symbol "~" *> expression)
  <|> Not <$> (symbol "!" *> expression)

returnStatement :: Parser Statement
returnStatement = fmap Return $ symbol "return" *> expression <* symbol ";"

statement :: Parser Statement
statement = returnStatement

body :: Parser Body
body = between (symbol "{") (symbol "}") $ many statement

type_ :: Parser Type
type_ = symbol "int"

parameters :: Parser Parameters
parameters = between (symbol "(") (symbol ")") $ pure []

fdef :: Parser FunctionDefinition
fdef = Fdef <$> type_ <*> identifier <*> parameters <*> body

class Assembly a where
  assembly :: a -> [Text]

instance Assembly FunctionDefinition where
  assembly (Fdef returnType name parameters body)
    = ".globl " <> name : name <> ":" : assembly body

instance Assembly Expression where
  assembly (Term (Literal (Integer i)))
    = [ "movq $" <> tshow i <> ", %rax" ]
  assembly (Negate e) = assembly e <> [ "neg %rax" ]
  assembly (Invert e) = assembly e <> [ "not %rax" ]
  assembly (Not e) = assembly e
    <> [ "cmpq $0, %rax"
       , "movq $0, %rax"
       , "sete %al"
       ]

instance Assembly Statement where
  assembly (Return e) = assembly e <> [ "ret" ]

instance Assembly a => Assembly [a] where
  assembly = concatMap assembly
