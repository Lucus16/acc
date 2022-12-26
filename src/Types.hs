{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Types where

import Control.Monad.Reader (ReaderT)
import Control.Monad.Writer (WriterT)
import Data.Map (Map)
import Data.Text (Text)
import Data.Word (Word64)
import Text.Megaparsec.Pos (SourcePos)
import Data.Binary.Writer qualified as Binary

-- 1. Parse to AST :: Text -> AST Text
-- 2. Resolve identifiers :: AST Text -> Map Symbol (Definition Symbol)
-- 3. Type check :: Map Symbol (Definition Symbol) -> Map Symbol (Definition Symbol)
-- 4. Linearize :: Map Symbol (Definition Symbol) -> Map Symbol [Conditional Symbol]
-- 5. Assign registers :: Map Symbol [Conditional Symbol] -> Map Symbol [Conditonal Symbol]
-- 6. Compute labels :: Map Symbol [Conditional Symbol] -> ByteString

type Size = Word64

newtype Id = Id Int deriving (Eq, Ord)

-- A Symbol has a unique identifier to disambiguate it from other identically named
-- symbols. Only the id field is used in comparisons, the other fields are just
-- to look up related information.
data Symbol = Symbol
  { symbolId :: !Id
  , name     :: !Text
  , src      :: !SourcePos
  }

instance Eq Symbol where
  Symbol{ symbolId = x } == Symbol{ symbolId = y } = x == y

instance Ord Symbol where
  compare Symbol{ symbolId = x } Symbol{ symbolId = y } = compare x y

data Type
  = Int8 | Int16 | Int32 | Int64
  | Word8 | Word16 | Word32 | Word64
  deriving (Show)

data Typed a = Typed Type a

getType :: Typed a -> Type
getType (Typed t _) = t

unTyped :: Typed a -> a
unTyped (Typed _ x) = x

data Literal = Integer Integer

-- ### Label resolution

-- The output of the writer monad is recursively inserted as the input of the
-- reader monad.
type Resolver = Binary.WriterT (WriterT (Map Id Word64) (ReaderT (Map Id Word64) (Either Text)))

data RegisterAssignedReference reg
  = OffsetTo Symbol
  | Literal  Integer
  | Register reg

-- ### Elf generation

-- This indicates which Elf section the blob will be placed in.
data BlobType
  = BlobTypeFunction
  | BlobTypeConstant
  | BlobTypeInitializedGlobal
  | BlobTypeZeroedGlobal

data Blob = Blob
  { label    :: Text
  , typ      :: BlobType
  , address  :: Size
  , size     :: Size
  , contents :: Binary.Tree
  }

-- The difference between signed and unsigned is indicated by the Types of id,
-- which have been checked to match after type checking. Checks if a value is
-- zero or negative are implemented by passing a literal in the second id.
data Condition id
  = Equal          id id
  | NotEqual       id id
  | LessThan       id id
  | LessOrEqual    id id
  | GreaterThan    id id
  | GreaterOrEqual id id
  | BitsSet        id id
  | BitsCleared    id id
  deriving (Foldable, Functor)

data Conditional id
  = Always (Operation id)
  | When (Condition id) (Operation id)
  | DefineLabel Id

data Operation id
  = Load        { dst :: id, addr :: id }
  | Store       { src :: id, addr :: id }
  | Jump        { addr :: id }
  | Call        { addr :: id }
  | Return      { addr :: id }
  | Compute     { dst :: id, computation :: Computation id }
  | Reinterpret { dst :: id, src :: id }

data Computation id
  = Copy id
  | Negate id
  | Invert id
  | Add id id
  | Subtract id id
  | Multiply id id
  | Divide id id
  | Modulo id id
  | And id id
  | AndNot id id
  | Or id id
  | OrNot id id
  | Xor id id
  | ShiftLeft id id
  | ShiftRight id id
  | RotateLeft id id
  | RotateRight id id
