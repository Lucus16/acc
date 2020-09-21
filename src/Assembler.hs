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

--newtype Assembler a = Assembler { runAssembler :: Int -> [Text] -> (Int, [Text], a) }
--
--instance Functor Assembler where
--  fmap f asm@Assembler{r} = asm { r = f r }
--  fmap f (Assembler runR) = Assembler $ \i a ->
--    let (i', a', r) = runR i a
--     in (i', a', f r)
--
--instance Applicative Assembler where
--  pure r = Assembler (\(i, a) -> (i, a, r))
--  Assembler runF <*> Assembler runX = Assembler $ \i a ->
--    let (i', a', f) = runF i a
--        (i'', a'', x) = runX i' a'
--     in (i'', a'', f x)
--
--instance Monad Assembler where
--  Assembler runX >>= f = Assembler $ \i a ->
--    let (i', a', x) = runX i a
--     in runAssembler (f x) i' a'

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
