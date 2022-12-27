{-# LANGUAGE TypeFamilies #-}

module Arch.Aarch64 where

import Data.Word (Word8, Word64)

import Arch.Class

data Aarch64 = Aarch64

instance Arch Aarch64 where
  type Pointer Aarch64 = Word64
  type AddressableUnit Aarch64 = Word8
  data Register Aarch64
    = X0 | X1 | X2 | X3 | X4 | X5 | X6 | X7
    | X8 | X9 | X10 | X11 | X12 | X13 | X14 | X15
    | X16 | X17 | X18 | X19 | X20 | X21 | X22 | X23
    | X24 | X25 | X26 | X27 | X28 | X29 | X30
    deriving (Enum)
