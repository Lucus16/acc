-- This module defines a lazy binary writer monad where values can depend on
-- offsets ahead of the data being written using recursive do notation.

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Data.Binary.Writer
  ( Tree(..)
  , WriterT(..)
  , Writer
  , execWriterT
  , execWriter
  , alignTo
  , getOffset
  , getSize
  , sub
  , getChunk
  , word8
  , word16
  , word32
  , word64
  , int8
  , int16
  , int32
  , int64
  , string
  , cString
  , byteString
  , lazyByteString
  , zeroes
  ) where

import Control.Monad.Fix (MonadFix, mfix)
import Control.Monad.Trans.Class (MonadTrans(..))
import Data.Binary qualified as Binary
import Data.Binary.Put qualified as Binary
import Data.ByteString qualified as BS
import Data.ByteString.Builder (Builder)
import Data.ByteString.Builder qualified as BSB
import Data.ByteString.Lazy qualified as BSL
import Data.Functor ((<&>))
import Data.Functor.Identity (Identity, runIdentity)
import Data.Word (Word8, Word16, Word32, Word64)
import Data.Int (Int8, Int16, Int32, Int64)
import Data.ByteString.Lazy.UTF8 as BSLU

-- We need to aggregate chunk completely lazily, otherwise we might introduce an
-- unnecessary circular dependency.
data Tree = Chunk BSL.ByteString | Node Tree Tree

instance Semigroup Tree where
  (<>) = Node

instance Monoid Tree where
  mempty = Chunk ""

treeToBuilder :: Tree -> Builder
treeToBuilder (Chunk bs) = BSB.lazyByteString bs
treeToBuilder (Node l r) = treeToBuilder l <> treeToBuilder r

data WriterT m a = WriterT { runWriterT :: Word64 -> m (a, Word64, Tree) }

instance Functor m => Functor (WriterT m) where
  fmap f m = WriterT $ fmap (\(a, o, w) -> (f a, o, w)) . runWriterT m

instance Monad m => Applicative (WriterT m) where
  pure a = WriterT \offset -> pure (a, offset, mempty)
  mf <*> mx = WriterT \offset -> do
    (f, offset',  w)  <- runWriterT mf offset
    (x, offset'', w') <- runWriterT mx offset'
    pure (f x, offset'', w <> w')

instance Monad m => Monad (WriterT m) where
  mx >>= mf = WriterT \offset -> do
    (x, offset',  w)  <- runWriterT mx offset
    (r, offset'', w') <- runWriterT (mf x) offset'
    pure (r, offset'', w <> w')

instance MonadFix m => MonadFix (WriterT m) where
  mfix f = WriterT $ \offset -> mfix $ \x -> runWriterT (f (f1of3 x)) offset

instance MonadTrans WriterT where
  lift m = WriterT $ \offset -> do
    a <- m
    return (a, offset, mempty)

f1of3 :: (a, b, c) -> a
f1of3 (a, _, _) = a

type Writer = WriterT Identity

execWriterT :: Functor m => WriterT m a -> m Builder
execWriterT writerT = runWriterT writerT 0 <&>
  (\(_, _, w) -> treeToBuilder w)

execWriter :: Writer a -> Builder
execWriter = runIdentity . execWriterT

binary :: (Applicative m, Binary.Binary a) => Word64 -> (a -> Binary.Put) -> a -> WriterT m ()
binary size put a = WriterT \offset -> pure ((), offset + size, Chunk $ Binary.runPut $ put a)

word8 :: Applicative m => Word8 -> WriterT m ()
word8 = binary 1 Binary.putWord8

word16 :: Applicative m => Word16 -> WriterT m ()
word16 = binary 2 Binary.putWord16le

word32 :: Applicative m => Word32 -> WriterT m ()
word32 = binary 4 Binary.putWord32le

word64 :: Applicative m => Word64 -> WriterT m ()
word64 = binary 8 Binary.putWord64le

int8 :: Applicative m => Int8 -> WriterT m ()
int8 = binary 1 Binary.putInt8

int16 :: Applicative m => Int16 -> WriterT m ()
int16 = binary 2 Binary.putInt16le

int32 :: Applicative m => Int32 -> WriterT m ()
int32 = binary 4 Binary.putInt32le

int64 :: Applicative m => Int64 -> WriterT m ()
int64 = binary 8 Binary.putInt64le

string :: Applicative m => String -> WriterT m ()
string = lazyByteString . BSLU.fromString

cString :: Monad m => String -> WriterT m ()
cString s = string s >> word8 0

byteString :: Applicative m => BS.ByteString -> WriterT m ()
byteString = lazyByteString . BSL.fromStrict

lazyByteString :: Applicative m => BSL.ByteString -> WriterT m ()
lazyByteString bs = WriterT \offset -> pure ((), offset + fromIntegral (BSL.length bs), Chunk bs)

zeroes :: Applicative m => Word64 -> WriterT m ()
zeroes count = lazyByteString $ BSL.replicate (fromIntegral count) 0

getOffset :: Applicative m => WriterT m Word64
getOffset = WriterT \offset -> pure (offset, offset, mempty)

getSize :: Monad m => WriterT m () -> WriterT m Word64
getSize m = WriterT \offset -> do
  ((), offset', w) <- runWriterT m offset
  pure (offset' - offset, offset', w)

-- Embed data in an outer WriterT and receive the offset, size and data as
-- result.
getChunk :: Monad m => WriterT m () -> WriterT m (Word64, Word64, Tree)
getChunk inner = WriterT \offset -> do
  ((), offset', innerData) <- runWriterT inner offset
  pure ((offset, offset' - offset, innerData), offset', innerData)

withOffset :: Monad m => Word64 -> WriterT m a -> WriterT m a
withOffset initialOffset inner = WriterT \outerOffset -> do
  (result, innerOffset, innerData) <- runWriterT inner initialOffset
  pure (result, outerOffset + innerOffset - initialOffset, innerData)

-- | sub efficiently includes the output of an inner Writer in an outer Writer
-- where the offsets in the inner Writer do not see data previously written to
-- the outer Writer.
sub :: Monad m => WriterT m a -> WriterT m a
sub = withOffset 0

alignTo :: Monad m => Word64 -> WriterT m ()
alignTo align
  | align < 2 = pure ()
  | otherwise = do
      offset <- getOffset
      let padding = (((offset + align - 1) `div` align) * align) - offset
      byteString (BS.replicate (fromIntegral padding) 0)
