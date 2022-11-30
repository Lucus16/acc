{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Elf where

import Data.Binary.Writer qualified as Binary
import Data.ByteString.Lazy (ByteString)
import Data.ByteString.Builder qualified as BSB
import Data.Word (Word64)

alignment :: Word64
alignment = 0x4000

virtualMemoryOffset :: Word64
virtualMemoryOffset = 0x400000

data Simple = Simple
  { code :: ByteString
  , rodata :: ByteString
  }

build :: Simple -> ByteString
build Simple{code, rodata} = BSB.toLazyByteString $ Binary.execWriter mdo
  -- ELF header
  Binary.word8 0x7f >> Binary.string "ELF" -- magic bytes
  Binary.word8 2 -- 64-bit
  Binary.word8 1 -- little-endian
  Binary.word8 1 -- elf header version
  Binary.word8 0 -- system v abi
  Binary.word64 0 -- padding
  Binary.word16 2 -- executable
  Binary.word16 0xb7 -- aarch64
  Binary.word32 1 -- elf version
  Binary.word64 virtualMemoryOffset -- program entry position
  Binary.word64 programTableOffset -- program header table position
  Binary.word64 sectionTableOffset -- section header table position
  Binary.word32 0 -- architecture flags
  Binary.word16 0x40 -- elf header size
  Binary.word16 0x38 -- program header size
  Binary.word16 programHeaderCount -- program header count
  Binary.word16 0x28 -- section header size
  Binary.word16 sectionHeaderCount -- section header count
  Binary.word16 namesSectionIndex -- index of the names section in the section header table

  -- section header table
  sectionTableOffset <- Binary.getOffset
  -- names section header
  let namesSectionIndex = 0
  Binary.word32 namesNameOffset -- section name offset
  Binary.word32 3 -- section header type: string table
  Binary.word64 0 -- flags, unused
  Binary.word64 0 -- address, unused
  Binary.word64 namesSectionOffset
  Binary.word64 namesSectionSize
  -- code section header
  Binary.word32 codeNameOffset
  Binary.word32 1 -- section header type: program
  Binary.word64 6 -- flags: runtime_allocated | executable
  Binary.word64 $ toVirtual codeSectionOffset
  Binary.word64 codeSectionOffset
  Binary.word64 codeSectionSize
  -- rodata section header
  Binary.word32 rodataNameOffset
  Binary.word32 1 -- section header type: program
  Binary.word64 2 -- flags: runtime_allocated
  Binary.word64 $ toVirtual rodataSectionOffset
  Binary.word64 rodataSectionOffset
  Binary.word64 rodataSectionSize
  let sectionHeaderCount = 3

  -- segment header table
  programTableOffset <- Binary.getOffset
  let programHeaderCount = 1
  Binary.word32 1 -- segment type load
  Binary.word32 5 -- permission flags: readable | executable
  Binary.word64 0 -- offset
  Binary.word64 virtualMemoryOffset -- location in virtual address space to write this segment to
  Binary.word64 0 -- unused for System V ABI, physical address space location otherwise
  Binary.word64 mainSegmentSize -- number of bytes to copy
  Binary.word64 mainSegmentSize -- number of bytes to clear before copying
  Binary.word64 alignment -- alignment equal to size of memory page on M1

  -- names section
  namesSectionOffset <- Binary.getOffset
  Binary.cString ".shstrtab"
  namesNameOffset <- fromIntegral . subtract namesSectionOffset <$> Binary.getOffset
  Binary.cString ".text"
  codeNameOffset <- fromIntegral . subtract namesSectionOffset <$> Binary.getOffset
  Binary.cString ".rodata"
  rodataNameOffset <- fromIntegral . subtract namesSectionOffset <$> Binary.getOffset
  namesSectionSize <- subtract namesSectionOffset <$> Binary.getOffset

  Binary.alignTo alignment
  -- main segment sections
  mainSegmentOffset <- Binary.getOffset
  let toVirtual x = virtualMemoryOffset - mainSegmentOffset + x

  -- code section
  codeSectionOffset <- Binary.getOffset
  Binary.lazyByteString code
  codeSectionSize <- subtract codeSectionOffset <$> Binary.getOffset

  -- rodata section
  rodataSectionOffset <- Binary.getOffset
  Binary.lazyByteString rodata
  rodataSectionSize <- subtract rodataSectionOffset <$> Binary.getOffset

  mainSegmentSize <- subtract mainSegmentOffset <$> Binary.getOffset
  pure ()
