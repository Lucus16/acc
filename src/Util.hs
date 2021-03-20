module Util
  ( Error
  , nth
  , tshow
  ) where

import Data.Text (Text)
import qualified Data.Text as Text

type Error = Text

tshow :: Show a => a -> Text
tshow = Text.pack . show

nth :: Int -> Text
nth 1 = "first"
nth 2 = "second"
nth 3 = "third"
nth 4 = "fourth"
nth 5 = "fifth"
nth 6 = "sixth"
nth 7 = "seventh"
nth 8 = "eighth"
nth 9 = "ninth"
nth 10 = "tenth"
nth i
  | i `mod` 10 == 1 = tshow i <> "st"
  | i `mod` 10 == 2 = tshow i <> "nd"
  | i `mod` 10 == 3 = tshow i <> "rd"
  | otherwise = tshow i <> "th"
