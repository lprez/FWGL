{-# LANGUAGE TypeOperators, DataKinds, ConstraintKinds,
             TypeFamilies, FlexibleContexts #-}

module FWGL.Graphics.Custom (
        Object,
        (~~),
        unsafeJoin,
        nothing,
        geom,

        Program,
        program,
        global,
        globalDraw,
        globalTexture,
        globalTexSize,

        Layer,
        layer,
        combineLayers,
        subLayer,
        depthSubLayer,
        colorDepthSubLayer,
        subLayerInspect,
        depthSubLayerInspect,
        colorDepthSubLayerInspect,

        Geometry,
        AttrList(..),
        mkGeometry,

        Texture,
        mkTexture,
        textureURL,
        textureFile,

        Color(..),
        colorTex,

        module FWGL.Vector
) where

import Control.Applicative
import Data.Typeable
import Data.Word (Word8)
import FRP.Yampa
import FWGL.Backend (BackendIO, GLES)
import FWGL.Geometry
import FWGL.Graphics.Color
import FWGL.Graphics.Draw
import FWGL.Graphics.Types hiding (program)
import FWGL.Internal.GL (GLES, ActiveTexture)
import FWGL.Internal.TList
import FWGL.Shader.CPU
import FWGL.Shader.Program
import FWGL.Graphics.Texture
import FWGL.Vector

-- | An empty custom object.
nothing :: Object '[] '[]
nothing = ObjectEmpty

-- | A custom object with a specified 'Geometry'.
geom :: Geometry i -> Object '[] i
geom = ObjectMesh

-- | Sets a global variable (uniform) of an object.
global :: (Typeable g, UniformCPU c g) => g -> c
       -> Object gs is -> Object (g ': gs) is
global g c = globalDraw g $ return c

-- | Sets a global (uniform) of an object using a 'Texture'.
globalTexture :: (BackendIO, Typeable g, UniformCPU ActiveTexture g)
              => g -> Texture -> Object gs is -> Object (g ': gs) is
globalTexture g c = globalDraw g $ textureUniform c

-- | Sets a global (uniform) of an object using the dimensions of a 'Texture'.
globalTexSize :: (BackendIO, Typeable g, UniformCPU c g) => g -> Texture
              -> ((Int, Int) -> c) -> Object gs is -> Object (g ': gs) is
globalTexSize g t fc = globalDraw g $ fc <$> textureSize t

-- | Sets a global (uniform) of an object using the 'Draw' monad.
globalDraw :: (Typeable g, UniformCPU c g) => g -> Draw c
           -> Object gs is -> Object (g ': gs) is
globalDraw = ObjectGlobal

-- | Join two objects.
(~~) :: (Equal gs gs', Equal is is')
     => Object gs is -> Object gs' is'
     -> Object (Union gs gs') (Union is is')
(~~) = ObjectAppend

-- | Join two objects, even if they don't provide the same variables.
unsafeJoin :: (Equal gs'' (Union gs gs'), Equal is'' (Union is is'))
           => Object gs is -> Object gs' is' -> Object gs'' is''
unsafeJoin = ObjectAppend

-- | Associate an object with a program.
layer :: (Subset oi pi, Subset og pg)
      => Program pg pi -> Object og oi -> Layer
layer = Layer

-- | Combine some layers.
combineLayers :: [Layer] -> Layer
combineLayers = MultiLayer

-- | Generate a 1x1 texture.
colorTex :: GLES => Color -> Texture
colorTex c = mkTexture 1 1 [ c ]

-- | Use a 'Layer' as a 'Texture' on another.
subLayer :: Int                         -- ^ Texture width.
         -> Int                         -- ^ Texture height.
         -> Layer                       -- ^ Layer to draw on a 'Texture'.
         -> (Texture -> [Layer])        -- ^ Layers using the texture.
         -> Layer
subLayer w h l f = SubLayer [ColorLayer] w h False False l $
                        \[t] _ _ -> f t

-- | Use a 'Layer' as a depth 'Texture' on another.
depthSubLayer :: Int                         -- ^ Texture width.
              -> Int                         -- ^ Texture height.
              -> Layer                       -- ^ Layer to draw on a depth 'Texture'.
              -> (Texture -> [Layer])        -- ^ Layers using the texture.
              -> Layer
depthSubLayer w h l f = SubLayer [DepthLayer] w h False False l $
                                \[t] _ _ -> f t

-- | Combination of 'subLayer' and 'depthSubLayer'.
colorDepthSubLayer :: Int                               -- ^ Texture width.
                   -> Int                               -- ^ Texture height.
                   -> Layer                             -- ^ Layer to draw on a 'Texture'
                   -> (Texture -> Texture -> [Layer])   -- ^ Color, depth.
                   -> Layer
colorDepthSubLayer w h l f = SubLayer [ColorLayer, DepthLayer] w h
                                      False False l $
                                \[ct, dt] _ _ -> f ct dt

-- | Use a 'Layer' as a 'Texture' on another, reading the content of the
-- texture.
subLayerInspect :: Int                              -- ^ Texture width.
                -> Int                              -- ^ Texture height.
                -> Layer                            -- ^ Layer to draw on a 'Texture'.
                -> (Texture -> [Color] -> [Layer])  -- ^ Layers using the texture.
                -> Layer
subLayerInspect w h l f = SubLayer [ColorLayer] w h True False l $
                        \[t] (Just c) _ -> f t c

-- | Use a 'Layer' as a depth 'Texture' on another, reading the content of the
-- texture.
depthSubLayerInspect :: Int                              -- ^ Texture width.
                     -> Int                              -- ^ Texture height.
                     -> Layer                            -- ^ Layer to draw on a depth 'Texture'.
                     -> (Texture -> [Word8] -> [Layer])  -- ^ Layers using the texture.
                     -> Layer
depthSubLayerInspect w h l f = SubLayer [DepthLayer] w h False True l $
                                \[t] _ (Just d) -> f t d

-- | Combination of 'subLayerInspect' and 'depthSubLayerInspect'.
colorDepthSubLayerInspect :: Int         -- ^ Texture width.
                          -> Int         -- ^ Texture height.
                          -> Layer       -- ^ Layer to draw on a 'Texture'
                          -> (Texture -> Texture -> [Color] -> [Word8] ->
                              [Layer])   -- ^ Layers using the texture.
                          -> Layer
colorDepthSubLayerInspect w h l f = SubLayer [ColorLayer, DepthLayer] w h
                                      True True l $
                                \[ct, dt] (Just c) (Just d) -> f ct dt c d
