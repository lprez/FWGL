module FWGL.Graphics.Color where

import Data.Hashable
import Data.Word (Word8)

-- | An RGBA 32-bit color.
data Color = Color !Word8 !Word8 !Word8 !Word8 deriving (Eq, Show)

instance Hashable Color where
        hashWithSalt salt (Color r g b a) = hashWithSalt salt (r, g, b, a)

-- | Create a 'Color' with alpha set to 255.
visible :: Word8 -> Word8 -> Word8 -> Color
visible r g b = Color r g b 255

white :: Color
white = Color 255 255 255 255

black :: Color
black = Color 0 0 0 255

transparent :: Color
transparent = Color 0 0 0 0

red :: Color
red = Color 255 0 0 255

green :: Color
green = Color 0 255 0 255

blue :: Color
blue = Color 0 255 255 255

yellow :: Color
yellow = Color 255 255 0 255
