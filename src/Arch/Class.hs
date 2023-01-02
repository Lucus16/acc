{-# LANGUAGE TypeFamilies #-}

module Arch.Class where

import Types

class Arch arch where
  type Pointer arch
  type AddressableUnit arch
  data Register arch

  generateCode :: Conditional (Typed (RegisterAssignedReference (Register arch))) -> Resolver ()
