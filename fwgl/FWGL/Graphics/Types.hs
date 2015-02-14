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
        Mesh(..),
        Light(..),
        Object(..),
        Layer(..)
) where

import Control.Applicative
import Control.Monad.IO.Class
import Control.Monad.Trans.State
import Data.Typeable
import FWGL.Geometry
import FWGL.Graphics.Color
import FWGL.Internal.GL hiding (Program, Texture, UniformLocation)
import qualified FWGL.Internal.GL as GL
import FWGL.Internal.TList
import FWGL.Internal.Resource
import FWGL.Shader.CPU
import FWGL.Shader.Program
import FWGL.Vector

newtype UniformLocation = UniformLocation GL.UniformLocation

data DrawState = DrawState {
        program :: Maybe (Program '[] '[]),
        loadedProgram :: Maybe LoadedProgram,
        programs :: ResMap (Program '[] '[]) LoadedProgram,
        uniforms :: ResMap (LoadedProgram, String) UniformLocation,
        gpuMeshes :: ResMap (Geometry '[]) GPUGeometry,
        textureImages :: ResMap TextureImage LoadedTexture
}

newtype Draw a = Draw { unDraw :: StateT DrawState GL a }
        deriving (Functor, Applicative, Monad, MonadIO)

-- | A texture.
data Texture = TextureImage TextureImage
             | TextureLoaded LoadedTexture
             
data TextureImage = TexturePixels [Color] GLSize GLSize Int
                  | TextureURL String Int

data LoadedTexture = LoadedTexture GLSize GLSize GL.Texture

-- | A static or dinamic geometry.
data Mesh is where
        Empty :: Mesh '[]
        Cube :: Mesh Geometry3
        StaticGeom :: Geometry is -> Mesh is
        DynamicGeom :: Geometry is -> Geometry is -> Mesh is

data Light

-- | An object is a set of geometries associated with some uniforms.
data Object (gs :: [*]) (is :: [*]) where
        ObjectEmpty :: Object gs is
        ObjectMesh :: Mesh is -> Object gs is
        ObjectGlobal :: (Typeable g, UniformCPU c g) => g -> Draw c
                     -> Object gs is -> Object gs' is 
        ObjectAppend :: Object gs is -> Object gs' is' -> Object gs'' is''

{-
-- | An 'Element' is anything that can be converted to an 'Object'.
class Element o gs is | o -> gs is where
        object :: o -> Object gs is

instance Element (Object gs is) gs is where
        object = id
-}

-- | An object associated with a program.
data Layer = forall oi pi og pg. (Subset oi pi, Subset og pg)
                              => Layer (Program pg pi) (Object og oi)
           | SubLayer Int Int Layer (Texture -> [Layer])
