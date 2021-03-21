module SPL.Syntax where

import Data.Text (Text)

data Source = Source !FilePath !Int !Int !Int !Int
data Sourced a = Sourced
  { source :: Source
  , unSourced :: a
  } deriving (Functor)

instance Semigroup Source where
  lhs <> rhs = src
    where
      Source path startLine startCol _ _ = lhs
      Source _ _ _ endLine endCol = rhs
      src = Source path startLine startCol endLine endCol

data Builtin
  = Negate
  | Not
  | Add
  | Subtract
  | Multiply
  | Divide
  | Modulo
  | Equal
  | NotEqual
  | LessThan
  | LessOrEqual
  | GreaterThan
  | GreaterOrEqual
  | And
  | Or
  | Tuple
  | Prepend
  | Print
  | IsEmpty
  | EmptyList
  deriving (Show)

data Expr' id f
  = Integer Integer
  | Boolean Bool
  | Character Char
  | Call (f (Expr' id f)) [f (Expr' id f)]
  | Builtin Builtin [f (Expr' id f)]
  | Variable id
  | Access (f (Expr' id f)) id

type SourcedExpr id = Sourced (Expr' id Sourced)

data Statement id
  = If (SourcedExpr id) [Statement id] [Statement id]
  | While (SourcedExpr id) [Statement id]
  | Assign id [id] (SourcedExpr id)
  | Expression (SourcedExpr id)
  | Return (Maybe (SourcedExpr id))
  | Vardec (Maybe (Type id)) id (SourcedExpr id)
  | Fundec id [id] (Maybe (Type id)) [Sourced (Statement id)]
  | Comment Text
  | EmptyLine

data Class = Ord | Eq | Show | All deriving (Eq, Ord, Show)

data Type id
  = Bool
  | Char
  | For Class id (Type id)
  | FunctionOf [Type id] (Type id)
  | Int
  | ListOf (Type id)
  | TupleOf [Type id]
  | Typevar id
  | Void
  deriving (Show)
