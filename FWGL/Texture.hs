module FWGL.Texture (
        Texture(..),
        mkTexture,
        textureURL,
        textureHash
) where

import Data.Hashable

import FWGL.Graphics.Color

-- | A texture.
data Texture = TexturePixels [Color] Int Int Int | TextureURL String Int
        deriving (Show)

-- | Creates a 'Texture' from a list of pixels.
mkTexture :: Int      -- ^ Width.
          -> Int      -- ^ Height.
          -> [Color]  -- ^ List of pixels
          -> Texture
mkTexture w h ps = TexturePixels ps w h $ hash ps

-- | Creates a 'Texture' from an URL.
textureURL :: String  -- ^ URL
           -> Texture
textureURL url = TextureURL url $ hash url

textureHash :: Texture -> Int
textureHash (TexturePixels _ _ _ h) = h
textureHash (TextureURL _ h) = h

instance Hashable Texture where
        hashWithSalt salt tex = hashWithSalt salt $ textureHash tex

instance Eq Texture where
        (TexturePixels _ _ _ h) == (TexturePixels _ _ _ h') = h == h'
        (TextureURL _ h) == (TextureURL _ h') = h == h'
        _ == _ = False
