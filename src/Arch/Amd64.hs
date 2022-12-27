{-# LANGUAGE TypeFamilies #-}

module Arch.Amd64 where

import Data.Word (Word8, Word64)

import Arch.Class

data Amd64 = Amd64

instance Arch Amd64 where
  type Pointer Amd64 = Word64
  type AddressableUnit Amd64 = Word8
  data Register Amd64
    = Rax | Rcx | Rdx | Rbx | Rsp | Rbp | Rsi | Rdi
    | R8 | R9 | R10 | R11 | R12 | R13 | R14 | R15
    deriving (Enum)

