{-# LANGUAGE DeriveTraversable #-}

-- | Generic expressions.
module Expr where

import Data.Text (Text)

type Identifier = Text
type Type = Identifier
type Parameter = ()
type Parameters = [Parameter]

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
  | Ternary (Expression id) (Expression id) (Expression id)
  deriving (Foldable, Functor, Show, Traversable)

data Literal
  = Integer Integer
  deriving (Show)

