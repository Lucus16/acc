{-# language FlexibleInstances #-}

module SPL.Syntax
  ( Builtin(..)
  , Class(..)
  , Expr'(..)
  , Ident(..)
  , Source(..)
  , Sourced(..)
  , SourcedExpr
  , SourcedStatement
  , Statement(..)
  , Type(..)
  , operatorText
  ) where

import Data.Foldable (for_)
import Data.Function (on)
import Data.Text (Text)
import qualified Data.Text as Text

import Pretty
import Util (tshow)

newtype Ident = Ident Text deriving (Eq, Ord)

instance Pretty Ident where
  pretty (Ident t) = pretty t

instance Show Ident where
  show (Ident t) = show t

data Source = Source !FilePath !Int !Int !Int !Int | Internal

data Sourced a = Sourced
  { source :: Source
  , unSourced :: a
  } deriving (Functor)

instance Pretty Source where
  pretty (Source path startLine startCol _ _) = do
    pretty path >> text ":"
    pretty startLine >> text ":"
    pretty startCol
  pretty Internal = text "<internal>"

instance Eq a => Eq (Sourced a) where
  (==) = (==) `on` unSourced

instance Semigroup Source where
  lhs <> rhs = src
    where
      Source path startLine startCol _ _ = lhs
      Source _ _ _ endLine endCol = rhs
      src = Source path startLine startCol endLine endCol

data Expr' f id
  = Integer Integer
  | Boolean Bool
  | Character Char
  | Call (f (Expr' f id)) [f (Expr' f id)]
  | Builtin Builtin [f (Expr' f id)]
  | Variable id
  | Access (f (Expr' f id)) id
  deriving (Foldable, Functor, Traversable)

type SourcedExpr id = Sourced (Expr' Sourced id)

data Statement id
  = If (SourcedExpr id) [SourcedStatement id] [SourcedStatement id]
  | While (SourcedExpr id) [SourcedStatement id]
  | Assign id [id] (SourcedExpr id)
  | Expression (SourcedExpr id)
  | Return (Maybe (SourcedExpr id))
  | Vardec (Maybe (Type id)) id (SourcedExpr id)
  | Fundec id [id] (Maybe (Type id)) [SourcedStatement id]
  | Comment Text
  | EmptyLine

type SourcedStatement id = Sourced (Statement id)

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
  deriving (Eq, Foldable, Functor, Show, Traversable)

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
  | Prepend
  | Tuple
  | EmptyList
  deriving (Eq, Show)

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

braced :: Pretty a => a -> Printer ()
braced body = do
  text "{" >> line
  indentBy "\t" $ pretty body
  line >> text "}" >> emptyLine

parenthesized :: Pretty a => a -> Printer ()
parenthesized p = text "(" *> pretty p <* text ")"

arguments :: Pretty a => [a] -> Printer ()
arguments [] = text "()"
arguments (x:xs) = parenthesized $ pretty x >> for_ xs ((text "," >> space >>) . pretty)

binary :: Pretty id => Builtin -> SourcedExpr id -> SourcedExpr id -> Printer ()
binary op x y = pretty x >> space >> pretty op >> space >> pretty y

semicolon :: Printer ()
semicolon = text ";" >> line

instance Pretty a => Pretty (Sourced a) where
  pretty (Sourced _ x) = pretty x

instance Pretty Class where
  pretty = text . tshow

instance Pretty id => Pretty (Type id) where
  pretty Bool = text "Bool"
  pretty Char = text "Char"
  pretty Void = text "Void"
  pretty Int = text "Int"
  pretty (Typevar var) = pretty var
  pretty (For All _ typ) = pretty typ
  pretty (For cls var typ) =
    pretty cls >> space >> pretty var >> space >> text "=>" >> space >> pretty typ

  pretty (FunctionOf params result) = do
    for_ params (\param -> pretty param >> space)
    text "->" >> space >> pretty result

  pretty (ListOf typ) = text "[" >> pretty typ >> text "]"
  pretty (TupleOf typs) = arguments typs

instance Pretty Builtin where
  pretty = text . operatorText

instance Pretty id => Pretty (Expr' Sourced id) where
  pretty (Integer i) = pretty $ tshow i
  pretty (Boolean True) = text "True"
  pretty (Boolean False) = text "False"
  pretty (Character '\'') = text "'\\''"
  pretty (Character '\\') = text "'\\\\'"
  pretty (Character '\0') = text "'\\0'"
  pretty (Character '\a') = text "'\\a'"
  pretty (Character '\n') = text "'\\n'"
  pretty (Character '\r') = text "'\\r'"
  pretty (Character '\t') = text "'\\t'"
  pretty (Character c) = text "'" >> text (Text.singleton c) >> text "'"
  pretty (Variable var) = pretty var
  pretty (Access expr field) = pretty expr >> text "." >> pretty field
  pretty (Call fun args) = pretty fun >> arguments args
  pretty (Builtin Negate [x]) = text "-" >> pretty x
  pretty (Builtin Not [x]) = text "!" >> pretty x
  pretty (Builtin EmptyList []) = text "[]"
  pretty (Builtin Tuple [arg]) = pretty arg
  pretty (Builtin Tuple args) = arguments args
  pretty (Builtin op [x, y]) = binary op x y
  pretty (Builtin builtin args) = error $
    show builtin <> " does not take " <> show (length args) <> " arguments."


instance Pretty id => Pretty (Statement id) where
  pretty (Comment c) = text "#" >> text (Text.stripEnd c) >> line
  pretty EmptyLine = emptyLine
  pretty (Fundec name params mbType body) = do
    pretty name
    arguments params
    space
    case mbType of
      Nothing -> pure ()
      Just typ -> do
        text "::" >> space
        pretty typ >> space
    braced body

  pretty (Vardec mbType name body) = do
    maybe (text "var") pretty mbType >> space
    pretty name >> space >> text "=" >> space >> pretty body >> semicolon

  pretty (Assign var fields value) = do
    pretty var
    for_ fields ((text "." >> ) . pretty)
    space >> text "=" >> space >> pretty value >> semicolon

  pretty (Expression expr) = pretty expr >> semicolon

  pretty (Return mbExpr) =
    text "return" >> for_ mbExpr ((space >>) . pretty) >> semicolon

  pretty (If cond t f) = do
    text "if" >> space >> parenthesized cond >> space >> braced t
    case f of
      [] -> pure ()
      body -> space >> text "else" >> space >> braced body

  pretty (While cond body) =
    text "while" >> space >> parenthesized cond >> space >> braced body
