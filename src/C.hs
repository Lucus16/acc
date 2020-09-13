{-# LANGUAGE OverloadedStrings #-}

module C where

import Prelude hiding (product, sum)

import Control.Applicative ((<|>))
import Data.Char (isAlpha, isDigit, isSpace)
import Data.Function ((&))
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

data Unary
  = Inv
  | Neg
  | Not
  deriving (Show)

data Binary
  = Add
  | Sub
  | Mul
  | Div
  deriving (Show)

data Expression
  = Term Term
  | Unary Unary Expression
  | Binary Binary Expression Expression
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
integer = Integer <$> lexeme L.decimal

literal :: Parser Literal
literal = integer

term :: Parser Term
term = Literal <$> literal

parenthesized :: Parser a -> Parser a
parenthesized = between (symbol "(") (symbol ")")

binaryText :: Binary -> Text
binaryText Add = "+"
binaryText Sub = "-"
binaryText Mul = "*"
binaryText Div = "/"

trailingBinary :: Binary -> Parser Expression -> Parser (Expression -> Expression)
trailingBinary op sub = symbol (binaryText op) *> fmap (flip $ Binary op) sub

binarySequence :: Parser Expression -> Parser (Expression -> Expression) -> Parser Expression
binarySequence first rest = foldl (&) <$> first <*> many rest

sum :: Parser Expression
sum = binarySequence product (addition <|> subtraction)
  where
    addition    = trailingBinary Add product
    subtraction = trailingBinary Sub product

product :: Parser Expression
product = binarySequence unary (multiplication <|> division)
  where
    multiplication = trailingBinary Mul unary
    division       = trailingBinary Div unary

expression :: Parser Expression
expression = sum

unary :: Parser Expression
unary = Term <$> term
  <|> Unary Neg <$> (symbol "-" *> unary)
  <|> Unary Inv <$> (symbol "~" *> unary)
  <|> Unary Not <$> (symbol "!" *> unary)
  <|> parenthesized expression

returnStatement :: Parser Statement
returnStatement = fmap Return $ symbol "return" *> expression <* symbol ";"

statement :: Parser Statement
statement = returnStatement

body :: Parser Body
body = between (symbol "{") (symbol "}") $ many statement

type_ :: Parser Type
type_ = symbol "int"

parameters :: Parser Parameters
parameters = parenthesized $ pure []

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

  assembly (Unary op e) = assembly e <> assembly op

  assembly (Binary op l r)
    = case (unaryAssembly "rax" l, unaryAssembly "rcx" r) of
        (_, Just rasm) -> assembly l <> rasm <> assembly op
        (Just lasm, _) -> assembly r <> [ "movq %rax, %rcx" ] <> lasm <> assembly op
        (Nothing, Nothing) ->
          assembly r <> [ "push %rax" ]
            <> assembly l <> [ "pop %rcx" ]
            <> assembly op
  where
    unaryAssembly reg (Binary _ _ _) = Nothing
    unaryAssembly reg (Term (Literal (Integer i)))
      = Just [ "movq $" <> tshow i <> ", %" <> reg ]

    unaryAssembly reg (Unary Neg e) = (<>["neg %" <> reg]) <$> unaryAssembly reg e
    unaryAssembly reg (Unary Inv e) = (<>["not %" <> reg]) <$> unaryAssembly reg e
    unaryAssembly reg (Unary Not e) = Nothing

instance Assembly Unary where
  assembly Neg = [ "neg %rax" ]
  assembly Inv = [ "not %rax" ]
  assembly Not
    = [ "cmpq $0, %rax"
      , "movq $0, %rax"
      , "sete %al"
      ]

instance Assembly Binary where
  assembly Div = [ "cqo", "idivq %rcx" ]
  assembly Mul = [ "imul %rcx" ]
  assembly Add = [ "add %rcx, %rax" ]
  assembly Sub = [ "sub %rcx, %rax" ]

instance Assembly Statement where
  assembly (Return e) = assembly e <> [ "ret" ]

instance Assembly a => Assembly [a] where
  assembly = concatMap assembly
