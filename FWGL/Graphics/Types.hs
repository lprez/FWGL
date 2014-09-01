module FWGL.Graphics.Types (
        Geometry(..),
        Mesh(..),
        Light(..),
        Solid(..),
        Object(..),
        Scene
) where

import FWGL.Geometry
import FWGL.Vector

data Mesh = Cube | StaticGeom Geometry

data Light

data Solid = Solid Mesh M4 {- Tex -}

data Object = SolidObject Solid | Light Light | NoObject

type Scene = [Object]
