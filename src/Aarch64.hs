{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Aarch64 (compile) where

import Control.Monad.State (StateT, execStateT, modify)
import Data.Binary.Put qualified as Binary
import Data.Bits
import Data.Foldable (traverse_)
import Data.Map qualified as Map
import Data.Text (Text)
import Data.Word (Word32)
import Data.ByteString.Lazy (ByteString)

import qualified C
import Expr (Expression(..), Unary(..))
import qualified IR
import Util (Error)
import Elf qualified

data Register
  = X0 | X1 | X2 | X3 | X4 | X5 | X6 | X7
  | X8 | X9 | X10 | X11 | X12 | X13 | X14 | X15
  | X16 | X17 | X18 | X19 | X20 | X21 | X22 | X23
  | X24 | X25 | X26 | X27 | X28 | X29 | X30 | X31
  deriving (Enum)

data EmitState = EmitState
  { eLabelNum :: Int
  , eInstructions :: [Word32]
  , eBreak :: Maybe Text
  , eContinue :: Maybe Text
  }

type Emitter = StateT EmitState (Either Text)

runEmitter :: Emitter () -> Either Text [Word32]
runEmitter emitter = reverse . eInstructions <$> execStateT emitter initialEmitState
  where
    initialEmitState = EmitState
      { eLabelNum = 0
      , eInstructions = []
      , eBreak = Nothing
      , eContinue = Nothing
      }

emit :: Word32 -> Emitter ()
emit instruction = modify $ \s -> s { eInstructions = instruction : eInstructions s }

--newLabel :: Text -> Emitter Text
--newLabel name = do
--  labelNum <- gets eLabelNum
--  modify $ \s -> s { eLabelNum = labelNum + 1 }
--  pure $ name <> tshow labelNum
--
--label :: Text -> Emitter ()
--label name = emit $ name <> ":"
--
--globalLabel :: Text -> Emitter ()
--globalLabel name = do
--  emit $ "global " <> name
--  emit "align 8"
--  emit $ name <> ":"
--
--jmp :: Text -> Emitter ()
--jmp name = emit $ "jmp " <> name

class Aarch64 a where
  asm :: a -> Emitter ()

instance Aarch64 IR.Statement where
  asm (IR.Return e) = do
    asm e
    emit 0xd2800bc8 -- x8 = exit
    emit 0xd4000001 -- syscall

  asm _ = error "unimplemented"

instance Aarch64 IR.Expression where
  asm (Literal (C.Integer i))
    | i < 0x10000 = emit $ fromIntegral $ 0xd2800000 .|. (i `shiftL` 5)
    | otherwise   = error "unimplemented"

  asm (Unary Inv x) = asm x >> emit 0x2a2003e0
  asm (Unary Neg x) = asm x >> emit 0x4b0003e0
  asm (Unary Not x) = asm x >> emit 0xf100001f >> emit 0x9a9f17e0

  asm _ = error "unimplemented"

compile :: C.File -> Either Error ByteString
compile file = do
  ir <- C.irFile file
  let (funcs, _vars) = Map.partition isFunc ir
  let Just (IR.FunctionDefinition _ _ stmts) = Map.lookup "main" funcs

  instructions <- runEmitter do
    traverse_ asm stmts

  let code = Binary.runPut $ traverse_ Binary.putWord32le instructions

  pure $ Elf.build (Elf.Simple code mempty)
  where
    isFunc IR.FunctionDefinition { } = True
    isFunc _ = False
