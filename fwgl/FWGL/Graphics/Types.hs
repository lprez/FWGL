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
        SubLayerType(..)
) where

import Control.Applicative
import Control.Monad.IO.Class
import Control.Monad.Trans.State
import Data.Hashable
import Data.Vector (Vector)
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

-- | The state of the 'Draw' monad.
data DrawState = DrawState {
        program :: Maybe (Program '[] '[]),
        loadedProgram :: Maybe LoadedProgram,
        programs :: ResMap (Program '[] '[]) LoadedProgram,
        uniforms :: ResMap (LoadedProgram, String) UniformLocation,
        gpuMeshes :: ResMap (Geometry '[]) GPUGeometry,
        textureImages :: ResMap TextureImage LoadedTexture,
        activeTextures :: Vector (Maybe Texture)
}

-- | A monad that represents OpenGL actions with some state ('DrawState').
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

-- | An object associated with a program. It can also be a layer included in
-- another.
data Layer = forall oi pi og pg. (Subset oi pi, Subset og pg)
                              => Layer (Program pg pi) (Object og oi)
           | SubLayer SubLayerType Int Int Layer (Texture -> [Layer])

data SubLayerType = ColorSubLayer | DepthSubLayer deriving Eq

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
