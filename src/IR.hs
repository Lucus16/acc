{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE OverloadedStrings          #-}

module IR where

import Control.Monad (when)
import Control.Monad.Except (throwError)
import Control.Monad.State (StateT, evalStateT, get, gets, modify, put)
import Data.Foldable (for_)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)

import qualified Expr
import Util

type Block = [Statement]
type Expression = Expr.Expression Reference
type Identifier = Text
type Locals = Int
type Type = Identifier

data IterationCount
  = ZeroOrMore
  | OneOrMore
  deriving (Show)

data Statement
  = Break
  | Continue
  | Expression Expression
  | If Expression Block Block
  | Return Expression
  -- | Loop iterationCount condition body update
  | Loop IterationCount Expression Block Block
  deriving (Show)

data TopLevel
  = FunctionDefinition Type Identifier [Expr.Parameter] Locals Block
  deriving (Show)

data Signature
  = Signature Type [Type]
  deriving (Eq, Show)

-- | Things referred to by an identifier.
data Reference
  -- | Offset from the base pointer for a local variable.
  = BPOffset Int
  -- | Offset from the stack pointer for a function argument.
  | SPOffset Int
  | Label Text Signature
  deriving (Show)

data ReferenceState = ReferenceState
  { rReference :: Reference
  , rDepth :: Depth
  } deriving (Show)

type Depth = Int
type Scope = Map Text ReferenceState

data BuilderState = BuilderState
  { bScope :: Scope
  -- | The depth indicates the number of nested blocks we're in and is used to
  -- determined if we should fail on a variable being declared twice.
  , bDepth :: Depth
  , bNextOffset :: Int
  , bLargestOffset :: Int
  , bParamOffset :: Int
  , bDefinedFunctions :: Set Identifier
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

declareParam :: Text -> Builder ()
declareParam name = do
  BuilderState { bScope, bDepth, bParamOffset } <- get
  let refState = ReferenceState
        { rReference = BPOffset bParamOffset
        , rDepth = bDepth
        }

  modify $ \s -> s
    { bScope = Map.insert name refState bScope
    , bParamOffset = bParamOffset + 8
    }

declareLocal :: Text -> Builder ()
declareLocal name = do
  BuilderState { bScope, bDepth, bNextOffset, bLargestOffset } <- get
  case name `Map.lookup` bScope of
    Just refState | rDepth refState == bDepth -> throwError $ "declared twice: " <> tshow name
    _ -> pure ()

  let bNextOffset' = bNextOffset + 8
  let refState = ReferenceState
        { rReference = BPOffset (-bNextOffset)
        , rDepth = bDepth
        }

  modify $ \s -> s
    { bScope = Map.insert name refState bScope
    , bNextOffset = bNextOffset'
    , bLargestOffset = max bLargestOffset bNextOffset'
    }

lookup :: Text -> IR.Builder Reference
lookup name = do
  scope <- gets bScope
  case name `Map.lookup` scope of
    Nothing -> throwError $ "not declared: " <> tshow name
    Just s  -> pure $ rReference s

evalBuilder :: Builder a -> Either Text a
evalBuilder b = evalStateT b initialBuilderState
  where
    initialBuilderState = BuilderState
      { bScope = mempty
      , bDepth = 0
      , bNextOffset = 0
      , bLargestOffset = 0
      , bParamOffset = 0
      , bDefinedFunctions = mempty
      }

declareFunction :: Type -> Identifier -> [Expr.Parameter] -> IR.Builder ()
declareFunction returnType name params = do
  BuilderState { bScope, bDepth } <- get
  let sig = Signature returnType (map Expr.paramType params)
      insertIt = modify $ \s -> s
        { bScope = Map.insert name ReferenceState
          { rReference = Label name sig
          , rDepth = bDepth
          } bScope
        }

  case name `Map.lookup` bScope of
    Nothing -> insertIt
    Just ReferenceState { rDepth } | rDepth < bDepth -> insertIt
    Just ReferenceState { rReference = Label _ sig' }
      | sig == sig' -> pure ()
    Just ReferenceState { } -> error $ "declared twice: " <> show name

defineFunction
  :: Type
  -> Identifier
  -> [Expr.Parameter]
  -> IR.Builder Block
  -> IR.Builder TopLevel
defineFunction returnType name params bodyBuilder = do
  declareFunction returnType name params
  before <- get
  when (name `Set.member` bDefinedFunctions before) $ error $ "defined twice: " <> show name
  put before
    { bLargestOffset = 0
    , bNextOffset = 0
    , bParamOffset = 16
    , bDefinedFunctions = name `Set.insert` bDefinedFunctions before
    }

  for_ params $ \p -> declareParam (Expr.paramName p)

  body <- bodyBuilder
  locals <- gets bLargestOffset
  modify $ \s -> s { bDepth = bDepth before }
  pure $ FunctionDefinition returnType name params locals body
