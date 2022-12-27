{-# LANGUAGE TypeFamilies #-}

module Arch.Class where

class Arch arch where
  type Pointer arch
  type AddressableUnit arch
  data Register arch
