module FWGL.Graphics.Shapes where

import FWGL.Vector
import FWGL.Graphics.Types

cubeGeometry :: Geometry
cubeGeometry =
        Geometry {
                vertices = [ V3   1  (-1) (-1)
                           , V3   1  (-1)   1
                           , V3 (-1) (-1)   1
                           , V3 (-1) (-1) (-1)
                           , V3   1    1  (-1)
                           , V3   1    1    1
                           , V3 (-1)   1    1
                           , V3 (-1)   1  (-1) ],
                uvCoords = [],
                normals = [],
                elements = [ 1, 2, 3
                           , 7, 6, 5
                           , 0, 4, 5
                           , 1, 5, 6
                           , 6, 7, 3
                           , 0, 3, 7
                           , 0, 1, 3
                           , 4, 7, 5
                           , 1, 0, 5
                           , 2, 1, 6
                           , 2, 6, 3
                           , 4, 0, 7 ]
        }
