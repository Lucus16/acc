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
import Types (Literal(..))

data Register
  = X0 | X1 | X2 | X3 | X4 | X5 | X6 | X7
  | X8 | X9 | X10 | X11 | X12 | X13 | X14 | X15
  | X16 | X17 | X18 | X19 | X20 | X21 | X22 | X23
  | X24 | X25 | X26 | X27 | X28 | X29 | X30 | X31
  deriving (Enum)

fromReg :: Register -> Word32
fromReg = fromIntegral . fromEnum

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

-- Because aarch64 requires the stack pointer to be aligned to 16 bytes when
-- calling functions, using push and pop instructions is unwieldy. Instead, I
-- should preallocate enough temporary stack space to compute any expression.

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
  asm (Literal (Integer i))
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

-- | asmOperands places the lhs in rax and the rhs in rcx.
--asmOperands :: IR.Expression -> IR.Expression -> Emitter ()
--asmOperands l r = case (unaryAsm "rax" l, unaryAsm "rcx" r) of
--  (_, Just rasm) -> asm l >> rasm
--  (Just lasm, _) -> asm r >> emit "mov rcx, rax" >> lasm
--  (Nothing, Nothing) -> push r >> asm l >> emit "pop rcx"

literalAsm :: Register -> Literal -> Emitter ()
literalAsm reg (Integer value) = go False 0
  where
    partValue :: Int -> Word32
    partValue part = fromInteger $ shiftR value (16 * part) .&. 0xffff

    go :: Bool -> Int -> Emitter ()
    go False 4 = mov False 0
    go True  4 = pure ()
    go initialized part
      | partValue part == 0 = go initialized (succ part)
      | otherwise = mov initialized part >> go True (succ part)

    mov :: Bool -> Int -> Emitter ()
    mov keep part = emit $ 0xd2800000
      .|. shiftL (fromIntegral $ fromEnum keep) 29
      .|. fromIntegral (shiftL part 21)
      .|. shiftL (partValue part) 5
      .|. fromReg reg

-- Emit instructions to compute the expression using only the single given
-- register if possible.
unaryAsm :: Register -> Expression id -> Maybe (Emitter ())
unaryAsm reg (Literal lit) = Just $ literalAsm reg lit

unaryAsm reg (Unary Neg e) = do
  a <- unaryAsm reg e
  Just do
    a
    emit $ 0xcb0003e0 .|. fromReg reg .|. (fromReg reg `shiftL` 16)

unaryAsm reg (Unary Inv e) = do
  a <- unaryAsm reg e
  Just do
    a
    emit $ 0xaa2003e0 .|. fromReg reg .|. (fromReg reg `shiftL` 16)

unaryAsm _reg _ = Nothing
