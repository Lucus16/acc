{-# LANGUAGE OverloadedStrings #-}

module IR where

import Control.Monad (when)
import Control.Monad.Except (throwError)
import Control.Monad.State (StateT, evalStateT, gets, modify)
import Data.Map.Strict (Map)
import Data.Text (Text)
import qualified Data.Map.Strict as Map

import qualified Expr
import Util

type Scope = Map Text IR.BPOffset

data BuilderState = BuilderState
  { irbScope :: Scope
  }

type Builder a = StateT BuilderState (Either Text) a

block :: Builder a -> Builder a
block inner = do
  scope <- gets irbScope
  result <- inner
  modify $ \s -> s { irbScope = scope }
  pure result

declare :: Text -> Builder ()
declare name = do
  scope <- gets irbScope
  when (name `Map.member` scope) $ throwError $ "declared twice: " <> tshow name
  let offset = BPOffset $ Map.size scope * 8
  modify $ \s -> s { irbScope = Map.insert name offset scope }

lookup :: Text -> IR.Builder BPOffset
lookup name = do
  scope <- gets irbScope
  case name `Map.lookup` scope of
    Nothing -> throwError $ "not declared: " <> tshow name
    Just offset -> pure offset

runBuilder :: IR.Builder a -> Either Text a
runBuilder = flip evalStateT BuilderState { irbScope = Map.empty }

-- | Offset from the base pointer for a local variable.
newtype BPOffset = BPOffset Int deriving (Show)

type Block = [Statement]
type Expression = Expr.Expression BPOffset
type Identifier = Text
type Type = Identifier

data Reg
  = Rax
  | Rbx
  | Rcx
  | Rdx
  | Rbp
  | Rsp

data Statement
  = Expression Expression
  | Return Expression
  | If Expression Block Block
  deriving (Show)

data TopLevel
  = Fdef Type Identifier Expr.Parameters Block
  deriving (Show)
