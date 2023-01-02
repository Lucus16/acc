{-# LANGUAGE TypeFamilies #-}

module Arch.Cpu16 where

import Control.Monad.Except (throwError)
import Control.Monad.Trans (lift)
import Control.Monad.Writer (WriterT, execWriterT, tell)
import Data.Bits
import Data.Foldable (toList, traverse_)
import Data.Map qualified as Map
import Data.Word (Word8, Word16, Word64)

import Arch.Class
import Data.Binary.Writer qualified as Binary
import Types
import Util

data Cpu16 = Cpu16
type Reg = Register Cpu16

fromReg :: Reg -> Word16
fromReg = fromIntegral . fromEnum

instance Arch Cpu16 where
  type Pointer Cpu16 = Word16
  type AddressableUnit Cpu16 = Word16
  data Register Cpu16
    = R0 | R1 | R2 | R3 | R4 | R5 | R6 | R7
    | R8 | R9 | R10 | R11 | R12 | R13 | R14 | R15
    deriving (Enum)

  generateCode = cpu16

type Emitter = WriterT [Word16] Resolver

always :: Emitter () -> Resolver ()
always f = execWriterT f >>= traverse_ Binary.word16

conditional :: Emitter () -> Resolver ()
conditional f = execWriterT f >>= traverse_ (Binary.word16 . (.|. 0x8000))

asmOperation :: Operation (Typed (RegisterAssignedReference Reg)) -> Emitter ()
asmOperation _ = undefined

cond
  :: Word8
  -> RegisterAssignedReference Reg
  -> RegisterAssignedReference Reg
  -> Emitter ()
cond op (Register rx) (Register ry) = tell
  [ shiftL (fromIntegral op) 8 .|. shiftL (fromReg rx) 4 .|. fromReg ry ]

cond _ _ _ = undefined

-- The Cpu16 supports easy && by simply conditionally updating the branch flag.
-- This cannot currently be represented in the generic Conditional though.

asmCondition :: Type -> Condition (RegisterAssignedReference Reg) -> Emitter ()
asmCondition Word16 (NotEqual rx@(Register _) (Literal 0)) = cond 0x48 rx rx
asmCondition Int16  (NotEqual rx@(Register _) (Literal 0)) = cond 0x48 rx rx
asmCondition Word16 (Equal    rx@(Register _) (Literal 0)) = cond 0x49 rx rx
asmCondition Int16  (Equal    rx@(Register _) (Literal 0)) = cond 0x49 rx rx

asmCondition Word16 (BitsSet        x y) = cond 0x48 x y
asmCondition Int16  (BitsSet        x y) = cond 0x48 x y
asmCondition Word16 (BitsCleared    x y) = cond 0x49 x y
asmCondition Int16  (BitsCleared    x y) = cond 0x49 x y
asmCondition Word16 (Equal          x y) = cond 0x4A x y
asmCondition Int16  (Equal          x y) = cond 0x4A x y
asmCondition Word16 (NotEqual       x y) = cond 0x4B x y
asmCondition Int16  (NotEqual       x y) = cond 0x4B x y
asmCondition Word16 (LessThan       x y) = cond 0x4C x y
asmCondition Word16 (GreaterThan    x y) = cond 0x4C y x
asmCondition Word16 (GreaterOrEqual x y) = cond 0x4D x y
asmCondition Word16 (LessOrEqual    x y) = cond 0x4D y x
asmCondition Int16  (LessThan       x y) = cond 0x4E x y
asmCondition Int16  (GreaterThan    x y) = cond 0x4E y x
asmCondition Int16  (GreaterOrEqual x y) = cond 0x4F x y
asmCondition Int16  (LessOrEqual    x y) = cond 0x4F y x

asmCondition typ _ = lift $ lift $ throwError $ "type not supported: " <> tshow typ

asmCondition' :: Condition (Typed (RegisterAssignedReference Reg)) -> Emitter ()
asmCondition' condition = case map getType $ toList condition of
  [typ] -> asmCondition typ $ unTyped <$> condition
  types -> lift $ lift $ throwError $ "ambiguous types: " <> tshow types

cpu16
  :: Conditional (Typed (RegisterAssignedReference Reg))
  -> Resolver ()
cpu16 (When [] op) = always (asmOperation op)
cpu16 (When (condition : conditions) op) = do
  always (asmCondition' condition)
  traverse_ (conditional . asmCondition') conditions
  conditional (asmOperation op)
cpu16 (DefineLabel label) = Binary.getOffset >>= defineLabel label

defineLabel :: Id -> Word64 -> Resolver ()
defineLabel label = lift . tell . Map.singleton label
