{-# language FlexibleInstances #-}

module SPL.Printer where

import Data.Text (Text)
import qualified Data.Text as Text
import Control.Monad.State (State, gets, modify, evalState)
import Control.Monad.Writer (execWriterT, WriterT, tell)
import Data.Foldable (for_)

import qualified SPL.Syntax as SPL

import Util (tshow)

data Spacing = NoSpacing | Space | Line | EmptyLine

data PrinterState = PrinterState
  { psIndentation :: Text
  , psNextSpace :: Spacing
  }

type Printer = WriterT Text (State PrinterState)

class Pretty a where
  pretty :: a -> Printer ()

text :: Text -> Printer ()
text t
  | Text.null t = pure ()
  | otherwise   = do
      nextSpace <- gets psNextSpace
      case nextSpace of
        NoSpacing -> pure ()
        Space -> tell " "
        Line -> tell "\n" >> gets psIndentation >>= tell
        EmptyLine -> tell "\n\n" >> gets psIndentation >>= tell
      modify $ \s -> s { psNextSpace = NoSpacing }
      tell t

line :: Printer ()
line = modify $ \s -> s { psNextSpace = Line }

emptyLine :: Printer ()
emptyLine = modify $ \s -> s { psNextSpace = EmptyLine }

space :: Printer ()
space = modify $ \s -> s { psNextSpace = Space }

instance Pretty a => Pretty (Maybe a) where
  pretty Nothing  = pure ()
  pretty (Just x) = pretty x

instance Pretty a => Pretty [a] where
  pretty = mapM_ pretty

instance Pretty Text where
  pretty = text

indentBy :: Text -> Printer a -> Printer a
indentBy t body = do
  indent <- gets psIndentation
  modify $ \s -> s { psIndentation = indent <> t }
  result <- body
  modify $ \s -> s { psIndentation = indent }
  pure result

braced :: Pretty a => a -> Printer ()
braced body = do
  text "{" >> line
  indentBy "\t" $ pretty body
  line >> text "}" >> emptyLine

initialState :: PrinterState
initialState = PrinterState
  { psIndentation = ""
  , psNextSpace = NoSpacing
  }

render :: Pretty a => a -> Text
render x = evalState (execWriterT (pretty x)) initialState

parenthesized :: Printer a -> Printer a
parenthesized p = text "(" *> p <* text ")"

arguments :: Pretty a => [a] -> Printer ()
arguments [] = text "()"
arguments (x:xs) = parenthesized $ pretty x >> for_ xs ((text "," >> space >>) . pretty)

instance Pretty a => Pretty (SPL.Sourced a) where
  pretty (SPL.Sourced _ x) = pretty x

instance Pretty SPL.Class where
  pretty = text . tshow

instance Pretty id => Pretty (SPL.Type id) where
  pretty SPL.Bool = text "Bool"
  pretty SPL.Char = text "Char"
  pretty SPL.Void = text "Void"
  pretty SPL.Int = text "Int"
  pretty (SPL.Typevar var) = pretty var
  pretty (SPL.For SPL.All _ typ) = pretty typ
  pretty (SPL.For cls var typ) =
    pretty cls >> space >> pretty var >> space >> text "=>" >> space >> pretty typ

  pretty (SPL.FunctionOf params result) = do
    for_ params (\param -> pretty param >> space)
    text "->" >> space >> pretty result

  pretty (SPL.ListOf typ) = text "[" >> pretty typ >> text "]"
  pretty (SPL.TupleOf typs) = arguments typs

instance Pretty SPL.Builtin where
  pretty = text . SPL.operatorText

binary :: Pretty id => SPL.Builtin -> SPL.SourcedExpr id -> SPL.SourcedExpr id -> Printer ()
binary op x y = pretty x >> space >> pretty op >> space >> pretty y

semicolon :: Printer ()
semicolon = text ";" >> line

instance Pretty id => Pretty (SPL.Expr' id SPL.Sourced) where
  pretty (SPL.Integer i) = pretty $ tshow i
  pretty (SPL.Boolean True) = text "True"
  pretty (SPL.Boolean False) = text "False"
  pretty (SPL.Character '\'') = text "'\\''"
  pretty (SPL.Character '\\') = text "'\\\\'"
  pretty (SPL.Character '\0') = text "'\\0'"
  pretty (SPL.Character '\a') = text "'\\a'"
  pretty (SPL.Character '\n') = text "'\\n'"
  pretty (SPL.Character '\r') = text "'\\r'"
  pretty (SPL.Character '\t') = text "'\\t'"
  pretty (SPL.Character c) = text "'" >> text (Text.singleton c) >> text "'"
  pretty (SPL.Variable var) = pretty var
  pretty (SPL.Access expr field) = pretty expr >> text "." >> pretty field
  pretty (SPL.Call fun args) = pretty fun >> arguments args
  pretty (SPL.Builtin SPL.Negate [x]) = text "-" >> pretty x
  pretty (SPL.Builtin SPL.Not [x]) = text "!" >> pretty x
  pretty (SPL.Builtin SPL.EmptyList []) = text "[]"
  pretty (SPL.Builtin SPL.Tuple [arg]) = pretty arg
  pretty (SPL.Builtin SPL.Tuple args) = arguments args
  pretty (SPL.Builtin op [x, y]) = binary op x y
  pretty (SPL.Builtin builtin args) = error $
    show builtin <> " does not take " <> show (length args) <> " arguments."


instance Pretty id => Pretty (SPL.Statement id) where
  pretty (SPL.Comment c) = text "#" >> text (Text.stripEnd c) >> line
  pretty SPL.EmptyLine = emptyLine
  pretty (SPL.Fundec name params mbType body) = do
    pretty name
    arguments params
    space
    case mbType of
      Nothing -> pure ()
      Just typ -> do
        text "::" >> space
        pretty typ >> space
    braced body

  pretty (SPL.Vardec mbType name body) = do
    maybe (text "var") pretty mbType >> space
    pretty name >> space >> text "=" >> space >> pretty body >> semicolon

  pretty (SPL.Assign var fields value) = do
    pretty var
    for_ fields ((text "." >> ) . pretty)
    space >> text "=" >> space >> pretty value >> semicolon

  pretty (SPL.Expression expr) = pretty expr >> semicolon

  pretty (SPL.Return mbExpr) =
    text "return" >> for_ mbExpr ((space >>) . pretty) >> semicolon

  pretty (SPL.If c t f) = do
    text "if"
    parenthesized $ pretty c
    braced t
    case f of
      [] -> pure ()
      body -> space >> text "else" >> space >> braced body

  pretty (SPL.While c body) = do
    text "if"
    parenthesized $ pretty c
    braced body
