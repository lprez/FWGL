{-# LANGUAGE MultiParamTypeClasses #-}

module FWGL.Texture (
        Texture(..),
        LoadedTexture(..),
        mkTexture,
        textureURL,
        textureFile,
        textureHash
) where

import Data.Hashable

import FWGL.Backend.GLES (GLES)
import FWGL.Backend.IO
import FWGL.Graphics.Color
import FWGL.Internal.GL hiding (Texture)
import qualified FWGL.Internal.GL as GL
import FWGL.Internal.Resource

-- | A texture.
data Texture = TexturePixels [Color] GLSize GLSize Int
             | TextureURL String Int

newtype LoadedTexture = LoadedTexture GL.Texture

-- | Creates a 'Texture' from a list of pixels.
mkTexture :: GLES
          => Int      -- ^ Width.
          -> Int      -- ^ Height.
          -> [Color]  -- ^ List of pixels
          -> Texture
mkTexture w h ps = TexturePixels ps (fromIntegral w) (fromIntegral h) $ hash ps

-- | Creates a 'Texture' from an URL (JavaScript only).
textureURL :: String  -- ^ URL
           -> Texture
textureURL url = TextureURL url $ hash url

-- | Creates a 'Texture' from a file (Desktop only).
textureFile :: String -> Texture
textureFile = textureURL

textureHash :: Texture -> Int
textureHash (TexturePixels _ _ _ h) = h
textureHash (TextureURL _ h) = h

instance Hashable Texture where
        hashWithSalt salt tex = hashWithSalt salt $ textureHash tex

instance Eq Texture where
        (TexturePixels _ _ _ h) == (TexturePixels _ _ _ h') = h == h'
        (TextureURL _ h) == (TextureURL _ h') = h == h'
        _ == _ = False

instance (BackendIO, GLES) => Resource Texture LoadedTexture GL where
        loadResource = loadTexture
        unloadResource _ (LoadedTexture t) = deleteTexture t

loadTexture :: (BackendIO, GLES) => Texture -> GL LoadedTexture
loadTexture tex =
        do t <- createTexture
           case tex of
                   (TexturePixels ps w h _) -> setup t $
                           do arr <- liftIO $ encodeColors ps
                              texImage2DBuffer gl_TEXTURE_2D 0
                                               (fromIntegral gl_RGBA)
                                               w h 0
                                               gl_RGBA
                                               gl_UNSIGNED_BYTE
                                               arr
                   (TextureURL url _) ->
                           do ctx <- getCtx
                              liftIO $ loadImage url $ flip evalGL ctx .
                                \img -> setup t $
                                        texImage2DImage gl_TEXTURE_2D 0
                                                        (fromIntegral gl_RGBA)
                                                        gl_RGBA
                                                        gl_UNSIGNED_BYTE
                                                        img
           return $ LoadedTexture t
        where setup t act = do
                bindTexture gl_TEXTURE_2D t
                act :: GL ()
                param gl_TEXTURE_MAG_FILTER gl_LINEAR
                param gl_TEXTURE_MIN_FILTER gl_LINEAR
                param gl_TEXTURE_WRAP_S gl_REPEAT
                param gl_TEXTURE_WRAP_T gl_REPEAT
                bindTexture gl_TEXTURE_2D noTexture
              param :: GLEnum -> GLEnum -> GL ()
              param p v = texParameteri gl_TEXTURE_2D p $ fromIntegral v

