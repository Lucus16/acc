module Pretty
  ( Pretty(..)
  , Printer
  , emptyLine
  , indentBy
  , line
  , render
  , space
  , text
  ) where

import Data.Text (Text)
import qualified Data.Text as Text
import Control.Monad.State (MonadState, State, gets, modify, evalState)
import Control.Monad.Writer (MonadWriter, execWriterT, WriterT, tell)
import Data.Functor (void)

import Util (tshow)

data Spacing = NoSpacing | Space | Line | EmptyLine

data PrinterState = PrinterState
  { psIndentation :: Text
  , psNextSpace :: Spacing
  }

newtype Printer a = Printer
  { unPrinter :: WriterT Text (State PrinterState) a
  } deriving (Applicative, Functor, Monad, MonadState PrinterState, MonadWriter Text)

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

instance Pretty Int where
  pretty = text . tshow

instance Pretty Char where
  pretty = text . Text.singleton

instance Pretty (Printer a) where
  pretty = void

indentBy :: Text -> Printer a -> Printer a
indentBy t body = do
  indent <- gets psIndentation
  modify $ \s -> s { psIndentation = indent <> t }
  result <- body
  modify $ \s -> s { psIndentation = indent }
  pure result

initialState :: PrinterState
initialState = PrinterState
  { psIndentation = ""
  , psNextSpace = NoSpacing
  }

render :: Pretty a => a -> Text
render x = evalState (execWriterT $ unPrinter $ pretty x) initialState
