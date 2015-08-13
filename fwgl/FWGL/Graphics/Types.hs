{-# LANGUAGE GADTs, DataKinds, KindSignatures, TypeOperators,
             ExistentialQuantification, GeneralizedNewtypeDeriving #-}

module FWGL.Graphics.Types (
        Draw(..),
        DrawState(..),
        UniformLocation(..),
        Texture(..),
        TextureImage(..),
        LoadedTexture(..),
        Geometry(..),
        Object(..),
        Layer(..),
        RenderLayer(..),
        LayerType(..),
        renderColor,
        renderDepth,
        renderColorDepth,
        renderColorInspect,
        renderDepthInspect,
        renderColorDepthInspect,
) where

import Control.Applicative
import Control.Monad.IO.Class
import Control.Monad.Trans.State
import Data.Hashable
import Data.Vect.Float hiding (Vector)
import Data.Vector (Vector)
import Data.Typeable
import Data.Word (Word8)
import FWGL.Backend.IO (Canvas)
import FWGL.Geometry
import FWGL.Graphics.Color
import FWGL.Internal.GL hiding (Program, Texture, UniformLocation)
import qualified FWGL.Internal.GL as GL
import FWGL.Internal.TList
import FWGL.Internal.Resource
import FWGL.Shader.CPU
import FWGL.Shader.Program

newtype UniformLocation = UniformLocation GL.UniformLocation

-- | The state of the 'Draw' monad.
data DrawState = DrawState {
        currentProgram :: Maybe (Program '[] '[]),
        loadedProgram :: Maybe LoadedProgram,
        programs :: ResMap (Program '[] '[]) LoadedProgram,
        uniforms :: ResMap (LoadedProgram, String) UniformLocation,
        gpuMeshes :: ResMap (Geometry '[]) GPUGeometry,
        textureImages :: ResMap TextureImage LoadedTexture,
        activeTextures :: Vector (Maybe Texture),
        viewportSize :: (Int, Int),
        currentCanvas :: Canvas
}

-- | A state monad on top of 'GL'.
newtype Draw a = Draw { unDraw :: StateT DrawState GL a }
        deriving (Functor, Applicative, Monad, MonadIO)

-- | A texture.
data Texture = TextureImage TextureImage
             | TextureLoaded LoadedTexture
             deriving Eq
             
data TextureImage = TexturePixels [Color] GLSize GLSize Int
                  | TextureURL String Int

data LoadedTexture = LoadedTexture GLSize GLSize GL.Texture

-- | An object is a set of geometries associated with some uniforms. For
-- example, if you want to draw a rotating cube, its vertices, normals, etc.
-- would be the 'Geometry', the combination of the geometry and the value of the
-- model matrix would be the 'Object', and the combination of the object with
-- a 'Program' would be the 'Layer'. In fact, 'Object's are just descriptions
-- of the actions to perform to draw something. Instead, the Element types in
-- "FWGL.Graphics.D2" and "FWGL.Graphics.D3" represent managed (high level) objects,
-- and they are ultimately converted to them.
data Object (gs :: [*]) (is :: [*]) where
        ObjectEmpty :: Object gs is
        ObjectMesh :: Geometry is -> Object gs is
        ObjectGlobal :: (Typeable g, UniformCPU c g) => g -> Draw c
                     -> Object gs is -> Object gs' is 
        ObjectAppend :: Object gs is -> Object gs' is' -> Object gs'' is''

-- | An object associated with a program.
data Layer = forall oi pi og pg. (Subset oi pi, Subset og pg)
                              => Layer (Program pg pi) (Object og oi)
           | SubLayer (RenderLayer [Layer])
           | MultiLayer [Layer]

-- | Represents a 'Layer' drawn on a 'Texture'.
data RenderLayer a = RenderLayer [LayerType] Int Int
                                 Int Int Int Int
                                 Bool Bool Layer
                                 ([Texture] -> Maybe [Color] ->
                                  Maybe [Word8] -> a)

data LayerType = ColorLayer | DepthLayer deriving Eq

instance Hashable TextureImage where
        hashWithSalt salt tex = hashWithSalt salt $ textureHash tex

instance Eq TextureImage where
        (TexturePixels _ _ _ h) == (TexturePixels _ _ _ h') = h == h'
        (TextureURL _ h) == (TextureURL _ h') = h == h'
        _ == _ = False

instance GLES => Eq LoadedTexture where
        LoadedTexture _ _ t == LoadedTexture _ _ t' = t == t'

textureHash :: TextureImage -> Int
textureHash (TexturePixels _ _ _ h) = h
textureHash (TextureURL _ h) = h

-- | Render a 'Layer' in a 'Texture'.
renderColor :: Int                         -- ^ Texture width.
            -> Int                         -- ^ Texture height.
            -> Layer                       -- ^ Layer to draw on a 'Texture'.
            -> (Texture -> a)              -- ^ Function using the texture.
            -> RenderLayer a
renderColor w h l f = RenderLayer [ColorLayer, DepthLayer] w h 0 0 0 0
                                  False False l $ \[t, _] _ _ -> f t

-- | Render a 'Layer' in a depth 'Texture'
renderDepth :: Int              -- ^ Texture width.
            -> Int              -- ^ Texture height.
            -> Layer            -- ^ Layer to draw on a depth 'Texture'.
            -> (Texture -> a)   -- ^ Function using the texture.
            -> RenderLayer a
renderDepth w h l f = RenderLayer [DepthLayer] w h 0 0 0 0 False False l $
                                  \[t] _ _ -> f t

-- | Combination of 'renderColor' and 'renderDepth'.
renderColorDepth :: Int                         -- ^ Texture width.
                 -> Int                         -- ^ Texture height.
                 -> Layer                       -- ^ Layer to draw on a 'Texture'
                 -> (Texture -> Texture -> a)   -- ^ Color, depth.
                 -> RenderLayer a
renderColorDepth w h l f =
        RenderLayer [ColorLayer, DepthLayer] w h 0 0 0 0 False False l $
                    \[ct, dt] _ _ -> f ct dt

-- | Render a 'Layer' in a 'Texture', reading the content of the texture.
renderColorInspect
        :: Int                          -- ^ Texture width.
        -> Int                          -- ^ Texture height.
        -> Layer                        -- ^ Layer to draw on a 'Texture'.
        -> Int                          -- ^ First pixel to read X
        -> Int                          -- ^ First pixel to read Y
        -> Int                          -- ^ Width of the rectangle to read
        -> Int                          -- ^ Height of the rectangle to read
        -> (Texture -> [Color] -> a)    -- ^ Function using the texture.
        -> RenderLayer a
renderColorInspect w h l rx ry rw rh f =
        RenderLayer [ColorLayer, DepthLayer] w h rx ry rw rh True False l $
                    \[t, _] (Just c) _ -> f t c

-- | Render a 'Layer' in a depth 'Texture, reading the content of the texture.
-- Not supported on WebGL.
renderDepthInspect
        :: Int                          -- ^ Texture width.
        -> Int                          -- ^ Texture height.
        -> Layer                        -- ^ Layer to draw on a depth 'Texture'.
        -> Int                          -- ^ First pixel to read X
        -> Int                          -- ^ First pixel to read Y
        -> Int                          -- ^ Width of the rectangle to read
        -> Int                          -- ^ Height of the rectangle to read
        -> (Texture -> [Word8] -> a)    -- ^ Layers using the texture.
        -> RenderLayer a
renderDepthInspect w h l rx ry rw rh f =
        RenderLayer [DepthLayer] w h rx ry rw rh False True l $
                    \[t] _ (Just d) -> f t d

-- | Combination of 'renderColorInspect' and 'renderDepthInspect'. Not supported
-- on WebGL.
renderColorDepthInspect
        :: Int         -- ^ Texture width.
        -> Int         -- ^ Texture height.
        -> Layer       -- ^ Layer to draw on a 'Texture'
        -> Int         -- ^ First pixel to read X
        -> Int         -- ^ First pixel to read Y
        -> Int         -- ^ Width of the rectangle to read
        -> Int         -- ^ Height of the rectangle to read
        -> (Texture -> Texture -> [Color] -> [Word8] -> a)  -- ^ Layers using
                                                            -- the texture.
        -> RenderLayer a
renderColorDepthInspect w h l rx ry rw rh f =
        RenderLayer [ColorLayer, DepthLayer] w h rx ry rw rh True True l $
                    \[ct, dt] (Just c) (Just d) -> f ct dt c d
