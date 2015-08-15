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
        globalFramebufferSize,

        Layer,
        layer,
        combineLayers,
        subLayer,
        depthSubLayer,
        subRenderLayer,
        renderColor,
        renderDepth,
        renderColorDepth,
        renderColorInspect,
        renderDepthInspect,
        renderColorDepthInspect,

        Geometry,
        AttrList(..),
        mkGeometry,

        Texture,
        mkTexture,
        textureURL,
        textureFile,

        Color(..),
        colorTex,

        module Data.Vect.Float
) where

import Control.Applicative
import Data.Typeable
import Data.Vect.Float
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

-- | An empty custom object.
nothing :: Object '[] '[]
nothing = ObjectEmpty

-- | A custom object with a specified 'Geometry'.
geom :: Geometry i -> Object '[] i
geom = ObjectMesh

-- | Sets a global variable (uniform) of an object.
global :: (Typeable g, UniformCPU c g)
       => (a -> g)      -- ^ Any function that returns the GPU type of the
                        -- uniform, like the constructor. This argument is
                        -- actually ignored, it only provides the type.
       -> c             -- ^ Value
       -> Object gs is -> Object (g ': gs) is
global g c = globalDraw g $ return c

-- | Sets a global (uniform) of an object using a 'Texture'.
globalTexture :: (BackendIO, Typeable g, UniformCPU ActiveTexture g)
              => (a -> g) -> Texture -> Object gs is -> Object (g ': gs) is
globalTexture g c = globalDraw g $ textureUniform c

-- | Sets a global (uniform) of an object using the dimensions of a 'Texture'.
globalTexSize :: (BackendIO, Typeable g, UniformCPU c g) => (a -> g) -> Texture
              -> ((Int, Int) -> c) -> Object gs is -> Object (g ': gs) is
globalTexSize g t fc = globalDraw g $ fc <$> textureSize t

-- | Sets a global (uniform) of an object using the dimensions of the
-- framebuffer.
globalFramebufferSize :: (BackendIO, Typeable g, UniformCPU c g) => (a -> g)
                      -> (Vec2 -> c) -> Object gs is -> Object (g ': gs) is
globalFramebufferSize g fc = globalDraw g $ fc . tupleToVec <$>
                                            (viewportSize <$> drawState)
        where tupleToVec (x, y) = Vec2 (fromIntegral x) (fromIntegral y)

-- | Sets a global (uniform) of an object using the 'Draw' monad.
globalDraw :: (Typeable g, UniformCPU c g) => (a -> g) -> Draw c
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

-- | Use a 'Layer' as a 'Texture' on another. Based on 'renderColor'.
subLayer :: Int                         -- ^ Texture width.
         -> Int                         -- ^ Texture height.
         -> Layer                       -- ^ Layer to draw on a 'Texture'.
         -> (Texture -> [Layer])        -- ^ Layers using the texture.
         -> Layer
subLayer w h l = subRenderLayer . renderColor w h l

-- | Use a 'Layer' as a depth 'Texture' on another. Based on 'renderDepth'.
depthSubLayer :: Int                         -- ^ Texture width.
              -> Int                         -- ^ Texture height.
              -> Layer                       -- ^ Layer to draw on a
                                             -- depth 'Texture'.
              -> (Texture -> [Layer])        -- ^ Layers using the texture.
              -> Layer
depthSubLayer w h l = subRenderLayer . renderDepth w h l

-- | Generalized version of 'subLayer' and 'depthSubLayer'.
subRenderLayer :: RenderLayer [Layer] -> Layer
subRenderLayer = SubLayer
