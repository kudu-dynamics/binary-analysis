module Data.BinaryAnalysis where

import Data.Hashable
import Data.Text
import Data.Word
import GHC.Generics
import qualified Numeric

type BitWidth = Word64

newtype AddressWidth = AddressWidth BitWidth
  deriving (Eq, Ord, Generic)

newtype Address = Address Word64
  deriving (Eq, Ord, Num, Real, Enum, Integral, Generic)

instance Show Address where
  show (Address x) = showString "Address 0x" . Numeric.showHex x $ ""

instance Hashable Address

data Symbol
  = Symbol
      { _symbolName :: Text,
        _symbolRawName :: Text
      }
  deriving (Eq, Ord, Show, Generic)

instance Hashable Symbol