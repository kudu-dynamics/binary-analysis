module Data.BinaryAnalysis where

import GHC.Generics
import qualified Numeric
import Data.Hashable
import Data.Word

newtype Address = Address Word64
  deriving (Eq, Ord, Num, Real, Enum, Integral, Generic)

instance Show Address where
  show (Address x) =  showString "Address 0x" . Numeric.showHex x $ ""

instance Hashable Address