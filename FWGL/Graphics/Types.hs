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
import FWGL.Vector

data Mesh = Empty | Cube | StaticGeom Geometry | DynamicGeom Geometry Geometry

data Light

data Solid = Solid Mesh M4 {- Tex -}

data Object = SolidObject Solid | Light Light | NoObject

type Scene = [Object]

objectGeometry :: Object -> Geometry
objectGeometry (SolidObject (Solid (StaticGeom g) _)) = g
objectGeometry (SolidObject (Solid (DynamicGeom _ g) _)) = g
objectGeometry _ = error "objectGeometry: requires StaticGeom/DynamicGeom."
