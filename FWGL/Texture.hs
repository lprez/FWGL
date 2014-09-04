module FWGL.Texture (
        Texture(..),
        mkTexture
) where

import Data.Hashable

import FWGL.Graphics.Color

-- | A texture.
data Texture = Texture [Color] Int Int Int

-- | Creates a 'Texture' from a list of pixels.
mkTexture :: Int     -- ^ Width.
          -> Int     -- ^ Height.
          -> [Color] -- ^ List of pixels
          -> Texture
mkTexture w h ps = Texture ps w h $ hash ps

instance Hashable Texture where
        hashWithSalt salt (Texture _ _ _ h) = hashWithSalt salt h

instance Eq Texture where
        (Texture _ _ _ h) == (Texture _ _ _ h') = h == h'
