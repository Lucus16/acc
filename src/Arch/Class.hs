{-# LANGUAGE TypeFamilies #-}

module Arch.Class where

class Arch arch where
  type Pointer arch
  type AddressableUnit arch
  type Register arch
