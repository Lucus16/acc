{-# LANGUAGE TypeFamilies #-}

module Architecture where

import Data.Word (Word16)

import Arch.Class

data Cpu16 = Cpu16

data Cpu16Register
  = R0 | R1 | R2 | R3 | R4 | R5 | R6 | R7
  | R8 | R9 | R10 | R11 | R12 | R13 | R14 | R15
  deriving (Enum)

instance Arch Cpu16 where
  type Pointer Cpu16 = Word16
  type AddressableUnit Cpu16 = Word16
  type Register Cpu16 = Cpu16Register
