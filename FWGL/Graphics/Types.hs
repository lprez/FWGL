{-# LANGUAGE GADTs, DataKinds, KindSignatures, TypeOperators,
             ExistentialQuantification #-}

module FWGL.Graphics.Types (
        Geometry(..),
        Transformation(..),
        Mesh(..),
        Light(..),
        Object(..),
        Scene(..),
        (<>)
) where

import Data.Monoid
import Data.Typeable
import FWGL.Geometry
import FWGL.Internal.TList
import FWGL.Shader.CPU
import FWGL.Shader.Program
import FWGL.Texture
import FWGL.Vector

-- | A static or dinamic geometry.
data Mesh is where
        Empty :: Mesh '[]
        Cube :: Mesh Geometry3
        StaticGeom :: Geometry is -> Mesh is
        DynamicGeom :: Geometry is -> Geometry is -> Mesh is

-- | A transformation matrix.
newtype Transformation = Transformation M4

data Light

-- | An object is a set of geometries associated with some uniforms.
data Object (gs :: [*]) (is :: [*]) where
        ObjectEmpty :: Object gs is
        ObjectMesh :: Mesh is -> Object gs is
        ObjectTexture :: Texture -> Object g i -> Object (Texture2 ': g) i
        -- invece di ObjectTexture, un ObjectGlobal che dipende dal DrawState?
        ObjectGlobal :: (Typeable g, UniformCPU c g) => g -> c -> Object gs is
                     -> Object (g ': gs) is 
        ObjectAppend :: Object gs is -> Object gs' is'
                     -> Object (Union gs gs') (Union is is')

-- | An object associated with a program.
data Scene = forall oi pi og pg. (Subset oi pi, Subset og pg)
             => Scene (Program pg pi) (Object og oi)

instance Monoid Transformation where
        mempty = Transformation idMat
        mappend (Transformation t') (Transformation t) = Transformation $
                                                                mul4 t t'
