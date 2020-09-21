{-# LANGUAGE OverloadedStrings #-}

module C where

import Prelude hiding (product, sum)

import Control.Applicative ((<|>), empty)
import Data.Char (isAlpha, isDigit, isSpace)
import Data.Function ((&))
import Data.Functor (void)
import Data.Text (Text)
import Data.Void (Void)
import Text.Megaparsec (Parsec, ParsecT, Stream, Token, Tokens, between, many, satisfy, takeWhileP, takeWhile1P)
import qualified Text.Megaparsec.Char.Lexer as L (decimal, lexeme, symbol)
import qualified Data.Text as Text

import Util (tshow)
import Assembler (Asm, asm, emit, newLabel)

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
  | Mod
  | Eq
  | Neq
  | Lt
  | Leq
  | Gt
  | Geq
  | And
  | Or
  | Shl
  | Shr
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

trailingBinary :: Parser Expression -> Binary -> Parser (Expression -> Expression)
trailingBinary sub op = symbol (binaryText op) *> fmap (flip $ Binary op) sub

binarySequence :: Parser Expression -> [Binary] -> Parser Expression
binarySequence sub ops = foldl (&) <$> sub <*> many rest
  where rest = foldr (<|>) empty $ map (trailingBinary sub) ops

multiplicative :: Parser Expression
multiplicative = binarySequence unary [Mul, Div, Mod]

additive :: Parser Expression
additive = binarySequence multiplicative [Add, Sub]

shift :: Parser Expression
shift = binarySequence additive [Shl, Shr]

relational :: Parser Expression
relational = binarySequence shift [Lt, Leq, Gt, Geq]

equality :: Parser Expression
equality = binarySequence relational [Eq, Neq]

logicalAndExpression :: Parser Expression
logicalAndExpression = binarySequence equality [And]

logicalOrExpression :: Parser Expression
logicalOrExpression = binarySequence logicalAndExpression [Or]

expression :: Parser Expression
expression = logicalOrExpression

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

instance Asm FunctionDefinition where
  asm (Fdef returnType name parameters body) = do
    emit $ ".globl " <> name
    emit $ name <> ":"
    asm body

instance Asm Expression where
  asm (Term (Literal (Integer i))) = emit $ "movq $" <> tshow i <> ", %rax"
  asm (Unary op e) = asm e >> asm op
  asm (Binary And l r) = do
    end <- newLabel
    asm l
    emit "cmpl $0, %rax"
    emit $ "je " <> end
    asm r
    emit "cmpl $0, %rax"
    emit "movl $0, %rax"
    emit "setne %al"
    emit $ end <> ":"

  asm (Binary Or l r) = do
    end <- newLabel
    asm l
    emit "cmpl $0, %rax"
    emit $ "jne " <> end
    asm r
    emit "cmpl $0, %rax"
    emit $ end <> ":"
    emit "movl $0, %rax"
    emit "setne %al"

  asm (Binary op l r) = do
    case (unaryAsm "rax" l, unaryAsm "rcx" r) of
      (_, Just rasm) -> asm l >> rasm >> asm op
      (Just lasm, _) -> asm r >> emit "movq %rax, %rcx" >> lasm >> asm op
      (Nothing, Nothing) -> do
        asm r
        emit "push %rax"
        asm l
        emit "pop %rcx"
        asm op
    where
      unaryAsm reg (Binary _ _ _) = Nothing
      unaryAsm reg (Term (Literal (Integer i))) = Just $ do
        emit $ "movq $" <> tshow i <> ", %" <> reg

      unaryAsm reg (Unary Neg e) = do
        a <- unaryAsm reg e
        Just $ do
          a
          emit $ "neg %" <> reg

      unaryAsm reg (Unary Inv e) = do
        a <- unaryAsm reg e
        Just $ do
          a
          emit $ "not %" <> reg

      unaryAsm reg (Unary Not e) = Nothing

instance Asm Unary where
  asm Neg = emit "neg %rax"
  asm Inv = emit "not %rax"
  asm Not = do
    emit "cmpq $0, %rax"
    emit "movq $0, %rax"
    emit "sete %al"

compareAsm = emit "cmpq %rax, %rcx" >> emit "movq $0, %rax"

instance Asm Binary where
  asm Div = emit "cqo" >> emit "idivq %rcx"
  asm Mul = emit "imul %rcx"
  asm Add = emit "add %rcx, %rax"
  asm Sub = emit "sub %rcx, %rax"
  asm Eq  = compareAsm >> emit "sete %al"
  asm Neq = compareAsm >> emit "setne %al"
  asm Lt  = compareAsm >> emit "setl %al"
  asm Leq = compareAsm >> emit "setle %al"
  asm Gt  = compareAsm >> emit "setg %al"
  asm Geq = compareAsm >> emit "setge %al"

instance Asm Statement where
  asm (Return e) = asm e >> emit "ret"
