module FWGL.Graphics (
        module FWGL.Graphics.Types,
        V3(..),
        vec3,
        cube,
        geom,
        translate,
        rotX,
        rotY,
        rotZ,
        scale
) where

import FWGL.Graphics.Types
import FWGL.Vector

cube :: Float -> Object
cube edge = scale edge . SolidObject $ Solid Cube idMat

geom :: Geometry -> Object
geom g = SolidObject $ Solid (StaticGeom g) idMat

translate :: V3 -> Object -> Object
translate t = transform $ transMat t

rotX :: Float -> Object -> Object
rotX a = transform $ rotXMat a

rotY :: Float -> Object -> Object
rotY a = transform $ rotYMat a

rotZ :: Float -> Object -> Object
rotZ a = transform $ rotZMat a

scale :: Float -> Object -> Object
scale f = transform $ scaleMat (V3 f f f)

transform :: M4 -> Object -> Object
transform mat' (SolidObject (Solid mesh mat)) =
                SolidObject . Solid mesh $ mul4 mat' mat
transform _ o = o
