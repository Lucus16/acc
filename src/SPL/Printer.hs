{-# language FlexibleInstances #-}

module SPL.Printer where

import Data.Text (Text)
import qualified Data.Text as Text
import Control.Monad.State (State, gets, modify, evalState)
import Control.Monad.Writer (execWriterT, WriterT, tell)
import Data.Foldable (for_)

import qualified SPL.Syntax as SPL

import Util (tshow)

data Spacing = Line | EmptyLine

data PrinterState = PrinterState
  { psIndentation :: Text
  , psNextSpace :: Maybe Spacing
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
        Nothing -> pure ()
        Just Line -> tell "\n" >> gets psIndentation >>= tell
        Just EmptyLine -> tell "\n\n" >> gets psIndentation >>= tell
      modify $ \s -> s { psNextSpace = Nothing }
      tell t

line :: Printer ()
line = modify $ \s -> s { psNextSpace = Just Line }

emptyLine :: Printer ()
emptyLine = modify $ \s -> s { psNextSpace = Just EmptyLine }

space :: Printer ()
space = text " "

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

braced :: Printer a -> Printer a
braced body = do
  text "{" >> line
  result <- indentBy "\t" body
  line >> text "}" >> emptyLine
  pure result

initialState :: PrinterState
initialState = PrinterState
  { psIndentation = ""
  , psNextSpace = Nothing
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

instance Pretty id => Pretty (SPL.Expr' id SPL.Sourced) where
  pretty (SPL.Integer i) = pretty $ tshow i
  pretty (SPL.Boolean True) = text "True"
  pretty (SPL.Boolean False) = text "False"
  pretty (SPL.Variable var) = pretty var
  pretty (SPL.Access expr field) = pretty expr >> text "." >> pretty field
  pretty _ = pure ()

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
    braced $ pretty body

  pretty (SPL.Vardec mbType name body) = do
    maybe (text "var") pretty mbType >> space
    pretty name >> space >> text "=" >> space >> pretty body >> text ";"

  pretty (SPL.Return mbExpr) =
    text "return" >> for_ mbExpr ((space >>) . pretty) >> text ";"

  pretty _ = pure ()
