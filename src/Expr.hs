-- | Generic expressions.
module Expr where

import Data.Text (Text)

import Types (Literal)

type Identifier = Text

data Parameter typ id = Parameter
  { paramType :: typ
  , paramName :: id
  } deriving (Show)

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
  | Com
  deriving (Show)

data Expression id
  = Literal Literal
  | Variable id
  | Unary Unary (Expression id)
  | Binary Binary (Expression id) (Expression id)
  | Assignment id (Expression id)
  | Call (Expression id) [Expression id]
  | Ternary (Expression id) (Expression id) (Expression id)
  deriving (Foldable, Functor, Show, Traversable)
