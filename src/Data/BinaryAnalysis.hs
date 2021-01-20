module Data.BinaryAnalysis where

import Data.Aeson (FromJSON, ToJSON, ToJSONKey, FromJSONKey)
import Data.Hashable
import Data.Int (Int64)
import Data.Text
import Data.Word
import GHC.Generics
import qualified Numeric

newtype Bytes = Bytes Word64
  deriving (Eq, Ord, Read, Show, Generic, Enum)
  deriving newtype (Real, Integral, Num)
  deriving anyclass (Hashable, FromJSON, ToJSON)

newtype Bits = Bits Word64
  deriving (Eq, Ord, Read, Show, Generic, Enum)
  deriving newtype (Real, Integral, Num)
  deriving anyclass (Hashable, FromJSON, ToJSON)

toBits :: Bytes -> Bits
toBits (Bytes n) = Bits (8*n)

toBytes :: Bits -> Bytes
toBytes (Bits n) = Bytes (n `div` 8)

newtype ByteOffset = ByteOffset Int64
  deriving (Eq, Ord, Read, Show, Generic, Enum)
  deriving newtype (Real, Integral, Num)
  deriving anyclass (Hashable, FromJSON, ToJSON)

newtype BitOffset = BitOffset Int64
  deriving (Eq, Ord, Read, Show, Generic, Enum)
  deriving newtype (Real, Integral, Num)
  deriving anyclass (Hashable, FromJSON, ToJSON, ToJSONKey, FromJSONKey)

toBitOffset :: ByteOffset -> BitOffset
toBitOffset (ByteOffset n) = BitOffset (8*n)

toByteOffset :: BitOffset -> ByteOffset
toByteOffset (BitOffset n) = ByteOffset (n `div` 8)

newtype AddressWidth = AddressWidth {bits :: Bits}
  deriving (Eq, Ord, Read, Show, Generic, Enum)
  deriving newtype (Real, Integral, Num)
  deriving anyclass (Hashable, FromJSON, ToJSON)

newtype Address = Address Bytes
  deriving (Eq, Ord, Generic, Enum)
  deriving newtype (Real, Integral, Num)
  deriving anyclass (Hashable, FromJSON, ToJSON)

instance Show Address where
  show (Address (Bytes x)) = showString "Address 0x" . Numeric.showHex x $ ""

data Symbol
  = Symbol
      { _symbolName :: Text,
        _symbolRawName :: Text
      }
  deriving (Eq, Ord, Show, Generic)
  deriving anyclass (Hashable, FromJSON, ToJSON)
