module AST where

import Data.Text (Text)

type AssemblyLines = [Text]

type Identifier = Text

type Type = Identifier

type Body = [Statement]

type Parameter = ()

type Parameters = [Parameter]

data TopLevel
  = Fdef Type Identifier Parameters Body
  deriving (Show)

type File = [TopLevel]

data Statement
  = Return Expression
  | Declaration Identifier (Maybe Expression)
  | Expression Expression
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
  | Assignment Identifier Expression
  deriving (Show)

data Term
  = Literal Literal
  | Variable Identifier
  deriving (Show)

data Literal
  = Integer Integer
  deriving (Show)


