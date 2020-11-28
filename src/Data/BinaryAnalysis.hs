module Data.BinaryAnalysis where

import Data.Hashable
import Data.Int (Int64)
import Data.Text
import Data.Word
import GHC.Generics
import qualified Numeric
import Test.SmallCheck.Series (Serial, series, newtypeCons, decDepth, (<~>))

newtype Bytes = Bytes Word64
  deriving (Eq, Ord, Read, Show, Generic, Enum, Real, Integral, Num)

instance Monad m => Serial m Bytes where
  series = newtypeCons Bytes

instance Hashable Bytes

newtype Bits = Bits Word64
  deriving (Eq, Ord, Read, Show, Generic, Enum, Real, Integral, Num)

instance Monad m => Serial m Bits where
  series = newtypeCons Bits

instance Hashable Bits

toBits :: Bytes -> Bits
toBits (Bytes n) = Bits (8*n)

toBytes :: Bits -> Bytes
toBytes (Bits n) = Bytes (n `div` 8)

newtype ByteOffset = ByteOffset Int64
  deriving (Eq, Ord, Read, Show, Generic, Enum, Real, Integral, Num)

instance Monad m => Serial m ByteOffset where
  series = newtypeCons ByteOffset

instance Hashable ByteOffset

newtype BitOffset = BitOffset Int64
  deriving (Eq, Ord, Read, Show, Generic, Enum, Real, Integral, Num)

instance Monad m => Serial m BitOffset where
  series = newtypeCons BitOffset

instance Hashable BitOffset

toBitOffset :: ByteOffset -> BitOffset
toBitOffset (ByteOffset n) = BitOffset (8*n)

toByteOffset :: BitOffset -> ByteOffset
toByteOffset (BitOffset n) = ByteOffset (n `div` 8)


newtype AddressWidth = AddressWidth Bits
  deriving (Eq, Ord, Read, Show, Generic, Enum, Real, Integral, Num)

instance Monad m => Serial m AddressWidth where
  series = newtypeCons AddressWidth

instance Hashable AddressWidth

newtype Address = Address Bytes
  deriving (Eq, Ord, Generic, Enum, Real, Integral, Num)

instance Monad m => Serial m Address where
  series = newtypeCons Address

instance Show Address where
  show (Address (Bytes x)) = showString "Address 0x" . Numeric.showHex x $ ""

instance Hashable Address

data Symbol
  = Symbol
      { _symbolName :: Text,
        _symbolRawName :: Text
      }
  deriving (Eq, Ord, Show, Generic)

instance Monad m => Serial m Symbol where
  series = decDepth $ Symbol
    <$> newtypeCons pack
    <~> newtypeCons pack

instance Hashable Symbol
