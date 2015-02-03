{-# LANGUAGE MultiParamTypeClasses #-}

module FWGL.Graphics.Texture (
        mkTexture,
        textureURL,
        textureFile,
        textureLayer,
        textureHash,
        emptyTexture
) where

import Data.Hashable

import FWGL.Backend.GLES (GLES)
import FWGL.Backend.IO
import FWGL.Graphics.Color
import FWGL.Graphics.Types
import FWGL.Internal.GL hiding (Texture)
import qualified FWGL.Internal.GL as GL
import FWGL.Internal.Resource

-- | Creates a 'Texture' from a list of pixels.
mkTexture :: GLES
          => Int      -- ^ Width.
          -> Int      -- ^ Height.
          -> [Color]  -- ^ List of pixels
          -> Texture
mkTexture w h ps = TextureImage . TexturePixels ps (fromIntegral w)
                                                   (fromIntegral h) $ hash ps

-- | Creates a 'Texture' from an URL or a local file.
textureURL :: String  -- ^ URL
           -> Texture
textureURL url = TextureImage . TextureURL url $ hash url

-- | The same as 'textureURL'.
textureFile :: String -> Texture
textureFile = textureURL

-- | Creates a 'Texture' from a 'Layer'.
textureLayer :: GLES => Int -> Int -> Layer -> Texture
textureLayer w h l = TextureLayer l (fromIntegral w) (fromIntegral h)

textureHash :: TextureImage -> Int
textureHash (TexturePixels _ _ _ h) = h
textureHash (TextureURL _ h) = h

instance Hashable TextureImage where
        hashWithSalt salt tex = hashWithSalt salt $ textureHash tex

instance Eq TextureImage where
        (TexturePixels _ _ _ h) == (TexturePixels _ _ _ h') = h == h'
        (TextureURL _ h) == (TextureURL _ h') = h == h'
        _ == _ = False

instance (BackendIO, GLES) => Resource TextureImage LoadedTexture GL where
        loadResource i f = loadTextureImage i $ f . Right -- TODO: err check
        unloadResource _ (LoadedTexture _ _ t) = deleteTexture t

loadTextureImage :: (BackendIO, GLES) => TextureImage
                 -> (LoadedTexture -> GL ()) -> GL ()
loadTextureImage tex f =
        case tex of
                (TexturePixels ps w h _) -> (>>= f) $
                        do t <- emptyTexture
                           arr <- liftIO $ encodeColors ps
                           texImage2DBuffer gl_TEXTURE_2D 0
                                            (fromIntegral gl_RGBA)
                                            w h 0
                                            gl_RGBA
                                            gl_UNSIGNED_BYTE
                                            arr
                           return $ LoadedTexture (fromIntegral w)
                                                  (fromIntegral h)
                                                  t
                (TextureURL url _) ->
                        do ctx <- getCtx
                           liftIO $ loadImage url $ \(img, w, h) ->
                                flip evalGL ctx $ do
                                        t <- emptyTexture
                                        texImage2DImage gl_TEXTURE_2D 0
                                                        (fromIntegral gl_RGBA)
                                                        gl_RGBA
                                                        gl_UNSIGNED_BYTE
                                                        img
                                        f $ LoadedTexture (fromIntegral w)
                                                          (fromIntegral h)
                                                          t

emptyTexture :: GLES => GL GL.Texture
emptyTexture = do t <- createTexture
                  bindTexture gl_TEXTURE_2D t
                  param gl_TEXTURE_MAG_FILTER gl_LINEAR
                  param gl_TEXTURE_MIN_FILTER gl_LINEAR
                  param gl_TEXTURE_WRAP_S gl_REPEAT
                  param gl_TEXTURE_WRAP_T gl_REPEAT
                  return t
        where param :: GLES => GLEnum -> GLEnum -> GL ()
              param p v = texParameteri gl_TEXTURE_2D p $ fromIntegral v
