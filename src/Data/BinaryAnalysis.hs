module Data.BinaryAnalysis where

import Data.Hashable
import Data.Text
import Data.Word
import GHC.Generics
import qualified Numeric

newtype Bytes = Bytes Word64
  deriving (Eq, Ord, Read, Show, Generic, Enum, Real, Integral, Num)

instance Hashable Bytes

newtype Bits = Bits Word64
  deriving (Eq, Ord, Read, Show, Generic, Enum, Real, Integral, Num)

instance Hashable Bits

toBits :: Bytes -> Bits
toBits (Bytes n) = Bits (8*n)

toBytes :: Bits -> Bytes
toBytes (Bits n) = Bytes (n `div` 8)

newtype AddressWidth = AddressWidth Bytes
  deriving (Eq, Ord, Read, Show, Generic, Enum, Real, Integral, Num)

instance Hashable AddressWidth

newtype Address = Address Bytes
  deriving (Eq, Ord, Generic, Enum, Real, Integral, Num)

instance Show Address where
  show (Address (Bytes x)) = showString "Address 0x" . Numeric.showHex x $ ""

instance Hashable Address

data Symbol
  = Symbol
      { _symbolName :: Text,
        _symbolRawName :: Text
      }
  deriving (Eq, Ord, Show, Generic)

instance Hashable Symbol
