module FWGL.Graphics.Types (
        Geometry(..),
        Mesh(..),
        Light(..),
        Solid(..),
        Object(..),
        Scene,
        objectGeometry
) where

import FWGL.Geometry
import FWGL.Texture
import FWGL.Vector

data Mesh = Empty | Cube | StaticGeom Geometry | DynamicGeom Geometry Geometry

data Light

data Solid = Solid Mesh M4 Texture

data Object = SolidObject Solid | Light Light | NoObject

type Scene = [Object]

objectGeometry :: Object -> Geometry
objectGeometry (SolidObject (Solid (StaticGeom g) _ _)) = g
objectGeometry (SolidObject (Solid (DynamicGeom _ g) _ _)) = g
objectGeometry _ = error "objectGeometry: requires StaticGeom/DynamicGeom."
