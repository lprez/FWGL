module FWGL.Graphics (
        module FWGL.Graphics.Types,
        V3(..),
        vec3,
        nothing,
        cube,
        geom,
        translate,
        rotX,
        rotY,
        rotZ,
        scale,
        dynamicE,
        dynamic
) where

import FRP.Yampa
import FWGL.Graphics.Types
import FWGL.Vector

nothing :: Object
nothing = SolidObject $ Solid Empty idMat

cube :: Float -> Object
cube edg = scale edg . SolidObject $ Solid Cube idMat

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

dynamicG :: (Object -> a -> Event Object) -> Object -> SF a Object
dynamicG f i = flip sscan i $ \ oldObj inp ->
                case f oldObj inp of
                        Event (SolidObject (Solid (StaticGeom newG) mat)) ->
                                SolidObject (
                                        Solid (DynamicGeom (objectGeometry oldObj)
                                                           newG)
                                              mat
                                )
                        NoEvent -> case oldObj of
                                        SolidObject (
                                                Solid (DynamicGeom _ new) mat
                                                ) -> SolidObject (
                                                        Solid (StaticGeom new)
                                                              mat
                                                        )
                                        _ -> oldObj
                        _ -> error "dynamicG: not a Geometry."

dynamicE :: Object -> SF (Event Object) Object
dynamicE = dynamicG $ flip const

dynamic :: SF Object Object
dynamic =
        dynamicG (\ o n -> if objectGeometry o == objectGeometry n
                                then Event n
                                else NoEvent
        ) nothing
