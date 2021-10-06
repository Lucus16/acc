module Util
  ( Error
  , tshow
  ) where

import Data.Text (Text)
import Data.Text qualified as Text
--import Text.Megaparsec (SourcePos)

type Error = Text

--data Span = Span SourcePos SourcePos

tshow :: Show a => a -> Text
tshow = Text.pack . show
