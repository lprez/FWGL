module FWGL.Graphics.Types where

import Data.Word (Word16)
import FWGL.Vector

data Geometry = Geometry {
        vertices :: [V3],
        uvCoords :: [V2],
        normals :: [V3],
        elements :: [Word16]
}

data Mesh = Cube

data Light

data Solid = Solid Mesh M4 {- Tex -}

data Object = SolidObject Solid | Light Light | NoObject

type Scene = [Object]
