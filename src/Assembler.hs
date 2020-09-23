{-# LANGUAGE OverloadedStrings #-}

module Assembler 
  ( Asm
  , asm
  , compile
  , emit
  , newLabel
  ) where

import Data.Foldable (traverse_)
import Control.Monad.State (State, execState, modify, state)
import Data.Text (Text)
import qualified Data.Text as Text

import Util (tshow)

type Emitter = State (Int, [Text])

newLabel :: Emitter Text
newLabel = state $ \(i, a) -> ("anon" <> tshow i, (i + 1, a))

emit :: Text -> Emitter ()
emit line = modify $ \(i, a) -> (i, line:a)

class Asm a where
  asm :: a -> Emitter ()

instance Asm a => Asm [a] where
  asm = traverse_ asm

compile :: Asm a => a -> Text
compile x =
  let (_, lines) = execState (asm x) (0, [])
   in Text.unlines (reverse lines)
