module Bitmap (Pixel(..), Bitmap(..)) where

import           Data.Word (Word8)

data Pixel = Pixel {
  pixR :: Word8,
  pixG :: Word8,
  pixB :: Word8
}

data Bitmap = Bitmap {
  bWidth  :: Int,
  bHeight :: Int,
  bData   :: [Pixel]
}
