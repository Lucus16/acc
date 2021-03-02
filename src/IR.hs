{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module IR where

import Control.Monad.Except (throwError)
import Control.Monad.State (StateT, evalStateT, get, gets, modify, put)
import Data.Map.Strict (Map)
import Data.Text (Text)
import qualified Data.Map.Strict as Map

import qualified Expr
import Util

type Block = [Statement]
type Expression = Expr.Expression BPOffset
type Identifier = Text
type Locals = Int
type Type = Identifier

data Statement
  = Break
  | Continue
  | Expression Expression
  | If Expression Block Block
  | Return Expression
  | While Expression Block
  | DoWhile Block Expression
  deriving (Show)

data TopLevel
  = Fdef Type Identifier Expr.Parameters Locals Block
  deriving (Show)

-- | Offset from the base pointer for a local variable.
newtype BPOffset = BPOffset Int deriving (Eq, Num, Ord, Show)

type Depth = Int
type Scope = Map Text (Depth, BPOffset)

data BuilderState = BuilderState
  { bScope :: Scope
  , bDepth :: Int
  , bNextOffset :: BPOffset
  , bLargestOffset :: BPOffset
  }

type Builder = StateT BuilderState (Either Text)

block :: Builder a -> Builder a
block inner = do
  before <- get
  put before { bDepth = bDepth before + 1 }
  result <- inner
  innerLargestOffset <- gets bLargestOffset
  put before { bLargestOffset = innerLargestOffset }
  pure result

declare :: Text -> Builder ()
declare name = do
  BuilderState { bScope, bDepth, bNextOffset, bLargestOffset } <- get
  case name `Map.lookup` bScope of
    Just (depth, _offset) | depth == bDepth -> throwError $ "declared twice: " <> tshow name
    _ -> pure ()

  let bNextOffset' = bNextOffset + 8

  modify $ \s -> s
    { bScope = Map.insert name (bDepth, bNextOffset) bScope
    , bNextOffset = bNextOffset'
    , bLargestOffset = max bLargestOffset bNextOffset'
    }

lookup :: Text -> IR.Builder BPOffset
lookup name = do
  scope <- gets bScope
  case name `Map.lookup` scope of
    Nothing -> throwError $ "not declared: " <> tshow name
    Just (_depth, offset) -> pure offset

buildFdef :: Type -> Identifier -> Expr.Parameters -> IR.Builder Block -> Either Text TopLevel
buildFdef typ name params blockBuilder = evalStateT fdefBuilder initialBuilderState
  where
    initialBuilderState = BuilderState
      { bScope = Map.empty
      , bDepth = 0
      , bNextOffset = BPOffset 0
      , bLargestOffset = BPOffset 0
      }

    fdefBuilder = do
      body <- blockBuilder
      BPOffset locals <- gets bLargestOffset
      pure $ Fdef typ name params locals body
