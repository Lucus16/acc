{-# LANGUAGE NamedFieldPuns #-}

module IR where

import Control.Monad (when)
import Control.Monad.Except (throwError)
import Control.Monad.State (StateT, execStateT, get, gets, modify, put)
import Data.Foldable (for_)
import Data.Int (Int64)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)

import qualified Expr
import Util

type Block = [Statement]
type Expression = Expr.Expression Reference
type Identifier = Text
type StackUsage = Int

data Type
  = Int
  | Function Type [Type]
  deriving (Eq, Show)

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

data Value
  = Int64 Int64
  deriving (Eq, Show)

data Definition
  = FunctionDefinition Type StackUsage Block
  | GlobalDefinition Type Value
  deriving (Show)

-- | Things referred to by an identifier.
data Reference
  -- | Offset from the base pointer for a local variable.
  = BPOffset Int
  -- | Offset from the stack pointer for a function argument.
  | SPOffset Int
  | Label Text Type
  deriving (Show)

type Depth = Int

data ReferenceState = ReferenceState
  { rReference :: Reference
  , rDepth :: Depth
  } deriving (Show)

data BuilderState = BuilderState
  { bLocals :: Map Identifier ReferenceState
  , bGlobals :: Map Identifier Reference
  -- | The depth indicates the number of nested blocks we're in and is used to
  -- determined if we should fail on a variable being declared twice.
  , bDepth :: Depth
  , bNextOffset :: Int
  , bLargestOffset :: Int
  , bParamOffset :: Int
  , bDefinitions :: Map Identifier Definition
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
  BuilderState { bLocals, bDepth, bParamOffset } <- get
  let refState = ReferenceState
        { rReference = BPOffset bParamOffset
        , rDepth = bDepth
        }

  modify $ \s -> s
    { bLocals = Map.insert name refState bLocals
    , bParamOffset = bParamOffset + 8
    }

declareLocal :: Text -> Builder ()
declareLocal name = do
  BuilderState { bLocals, bDepth, bNextOffset, bLargestOffset } <- get
  case name `Map.lookup` bLocals of
    Just refState | rDepth refState == bDepth -> throwError $ "declared twice: " <> tshow name
    _ -> pure ()

  let bNextOffset' = bNextOffset + 8
  let refState = ReferenceState
        { rReference = BPOffset (-bNextOffset)
        , rDepth = bDepth
        }

  modify $ \s -> s
    { bLocals = Map.insert name refState bLocals
    , bNextOffset = bNextOffset'
    , bLargestOffset = max bLargestOffset bNextOffset'
    }

lookup :: Text -> IR.Builder Reference
lookup name = do
  BuilderState { bLocals, bGlobals } <- get
  case name `Map.lookup` bLocals of
    Just s  -> pure $ rReference s
    Nothing -> case name `Map.lookup` bGlobals of
      Just s  -> pure s
      Nothing -> throwError $ "not declared: " <> tshow name

evalBuilder :: Builder a -> Either Text (Map Identifier Definition)
evalBuilder b = bDefinitions <$> execStateT b initialBuilderState
  where
    initialBuilderState = BuilderState
      { bLocals = mempty
      , bGlobals = mempty
      , bDepth = 0
      , bNextOffset = 0
      , bLargestOffset = 0
      , bParamOffset = 0
      , bDefinitions = mempty
      }

defaultGlobals :: IR.Builder ()
defaultGlobals = do
  let zero = GlobalDefinition Int (Int64 0)
  vars <- gets $ Map.filter isVar . bGlobals
  modify $ \s -> s { bDefinitions = bDefinitions s `Map.union` fmap (const zero) vars }
  where
    isVar (Label _ Function { }) = False
    isVar (Label _ _)            = True
    isVar _                      = False

declareGlobal :: Identifier -> Type -> IR.Builder ()
declareGlobal name typ = do
  BuilderState { bGlobals } <- get
  case name `Map.lookup` bGlobals of
    Just (Label _ typ') | typ == typ' -> pure ()
    Just _ -> throwError $ "declared twice: " <> tshow name
    Nothing -> modify $ \s -> s
      { bGlobals = Map.insert name (Label name typ) bGlobals }

defineGlobal :: Identifier -> Type -> Value -> IR.Builder ()
defineGlobal name typ value = do
  declareGlobal name typ
  BuilderState { bDefinitions } <- get
  when (name `Map.member` bDefinitions) $ throwError $ "defined twice: " <> tshow name
  modify $ \s -> s { bDefinitions = Map.insert name (GlobalDefinition typ value) bDefinitions }

declareFunction :: Type -> Identifier -> [Expr.Parameter Type Identifier] -> IR.Builder ()
declareFunction returnType name params
  = declareGlobal name (Function returnType (map Expr.paramType params))

defineFunction
  :: Identifier
  -> Type
  -> [Expr.Parameter Type Identifier]
  -> IR.Builder Block
  -> IR.Builder ()
defineFunction name returnType params bodyBuilder = do
  declareFunction returnType name params
  before <- get
  when (name `Map.member` bDefinitions before) $ throwError $ "defined twice: " <> tshow name
  put before
    { bLargestOffset = 0
    , bNextOffset = 0
    , bParamOffset = 16
    }

  body <- block do
    for_ params $ declareParam . Expr.paramName
    bodyBuilder
  stackUsage <- gets bLargestOffset

  let definition = FunctionDefinition returnType stackUsage body
  modify $ \s -> s { bDefinitions = Map.insert name definition (bDefinitions s) }
