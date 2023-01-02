{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE RecursiveDo #-}

module ResolveOffsets where

import Control.Monad.Except (throwError)
import Control.Monad.Trans (lift)
import Control.Monad.Reader (asks)
import Data.Binary.Writer qualified as Binary
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Word (Word64)

import Types (Blob(..), BlobType(..), Conditional, Symbol(..), Resolver, Size, Type(..), Typed(..))
import Util (tshow)

--data LinearizedReference
--  = Parameter Symbol
--  | Local Symbol
--  | Global Symbol
--  | LocalLabel Symbol
--  | SubExpression Symbol
--  | GeneratedIntermediate Id
--  | FunctionRef Symbol
--  | Literal Literal

data RegisterAssignedReference reg
  = Global Symbol
  | LocalLabel Symbol
  | FunctionRef Symbol
  | Constant Symbol
  | Literal Integer
  | Register reg

-- TODO: Somehow ensure only relative offsets can be written.

getOffsetTo :: Symbol -> Resolver Word64
getOffsetTo Symbol{symbolId, name} = do
  srcOffset <- Binary.getOffset
  tgtOffset <- lift (asks (Map.lookup symbolId)) >>=
    maybe (lift $ throwError $ "position missing for " <> name) pure
  pure $ tgtOffset - srcOffset

writeReference
  :: Typed (RegisterAssignedReference reg)
  -> Resolver ()
writeReference (Typed typ reference) = case reference of
  Literal val     -> toType typ val
  Global ref      -> getOffsetTo ref >>= toType typ
  LocalLabel ref  -> getOffsetTo ref >>= toType typ
  FunctionRef ref -> getOffsetTo ref >>= toType typ
  Constant ref    -> getOffsetTo ref >>= toType typ
  _               -> lift $ throwError "absolute reference not supported"

data RegisterAssignedFunction reg = RegisterAssignedFunction
  [Conditional (Typed (RegisterAssignedReference reg))]

data UnresolvedBlobs reg = UnresolvedBlobs
  { functions :: Map Symbol [RegisterAssignedFunction reg]
  , constants :: Map Symbol (Typed (RegisterAssignedReference reg))
  -- The globals will be mapped into a read-write segment.
  , initializedGlobals :: Map Symbol (Typed (RegisterAssignedReference reg))
  , zeroedGlobals :: Map Symbol Size
  }

-- The length of an offset loading instruction sequence can depend on the value
-- of the offset. There is generally a worst case length and usually, the
-- instruction sequence gets shorter as the absolute value gets lower.

-- A simple algorithm to deal with this:
-- - generate code whose size does not depend on the offsets and note the offsets
-- - generate code assuming the offsets of the previous iteration
-- - repeat until the code does not grow smaller anymore

-- For constexprs whose absolute value might increase when the distance between
-- labels decreases, we must always assume the worst case or this algorithm
-- might not terminate.

-- Since the first step is to produce code whose size does not depend on the
-- offsets, I might as well begin by implementing only that. That should work
-- well with laziness and RecursiveDo.

-- We only use position-independent accesses so the initial offset does not
-- matter.
resolveOffsets :: UnresolvedBlobs reg -> Resolver (Map Symbol Blob)
resolveOffsets UnresolvedBlobs{functions, constants, initializedGlobals, zeroedGlobals} = mdo
  Binary.alignTo 16384 -- The next data goes to a writable page.
  constantBlobs <- Map.traverseWithKey getConstantBlob constants
  initializedBlobs <- Map.traverseWithKey getInitializedBlob initializedGlobals
  zeroedBlobs <- Map.traverseWithKey getZeroedBlob zeroedGlobals
  let allBlobs :: Map Symbol Blob
      allBlobs = Map.unions
        [ constantBlobs
        , initializedBlobs
        , zeroedBlobs
        ]
      --allOffsets = fmap (\Blob{address} -> address) allBlobs
  pure allBlobs

  where
    getConstantBlob = getBlob BlobTypeConstant writeReference
    getInitializedBlob = getBlob BlobTypeInitializedGlobal writeReference
    getZeroedBlob = getBlob BlobTypeZeroedGlobal Binary.zeroes

    getBlob :: BlobType -> (a -> Resolver ()) -> Symbol -> a -> Resolver Blob
    getBlob blobType f Symbol{name} value = do
      (address, size, contents) <- Binary.getChunk $ f value
      pure Blob
        { label = name
        , typ = blobType
        , address
        , size
        , contents
        }

toType :: (Integral val, Show val) => Type -> val -> Resolver ()
toType typ val = case typ of
  Word8  -> toTypeWith Binary.word8
  Word16 -> toTypeWith Binary.word16
  Word32 -> toTypeWith Binary.word32
  Word64 -> toTypeWith Binary.word64
  Int8   -> toTypeWith Binary.int8
  Int16  -> toTypeWith Binary.int16
  Int32  -> toTypeWith Binary.int32
  Int64  -> toTypeWith Binary.int64
  _      -> error $ "ambiguous type not resolved: " <> show typ
  where
    err = "value " <> tshow val <> " cannot be represented as " <> tshow typ

    toTypeWith
      :: Integral t
      => (t -> Resolver ())
      ->  Resolver ()
    toTypeWith writeT
      | val == val' = writeT tval
      | otherwise   = lift $ throwError err
      where
        tval = fromIntegral val
        val' = fromIntegral tval
