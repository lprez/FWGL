module FWGL.Graphics (
        Scene,
        Object,
        Geometry,
        V3(..),
        mkGeometry,
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
import FWGL.Geometry (mkGeometry)
import FWGL.Graphics.Types
import FWGL.Vector


-- | An empty object.
nothing :: Object
nothing = SolidObject $ Solid Empty idMat

-- | A cube.
cube :: Float  -- ^ Edge
     -> Object
cube edg = scale edg . SolidObject $ Solid Cube idMat

-- | An object with a specified 'Geometry'.
geom :: Geometry -> Object
geom g = SolidObject $ Solid (StaticGeom g) idMat

-- | Translate an object.
translate :: V3 -> Object -> Object
translate t = transform $ transMat t

-- | Rotate an object around the X axis.
rotX :: Float -> Object -> Object
rotX a = transform $ rotXMat a

-- | Rotate an object around the Y axis.
rotY :: Float -> Object -> Object
rotY a = transform $ rotYMat a

-- | Rotate an object around the Z axis.
rotZ :: Float -> Object -> Object
rotZ a = transform $ rotZMat a

-- | Scale an object.
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

-- | Like 'dynamic', but instead of comparing the two objects it checks the
-- event with the new object.
dynamicE :: Object -- ^ Initial 'Object'.
         -> SF (Event Object) Object
dynamicE = dynamicG $ flip const

-- | Automatically deallocate the previous mesh from the GPU when it changes.
dynamic :: SF Object Object
dynamic =
        dynamicG (\ o n -> if objectGeometry o == objectGeometry n
                                then Event n
                                else NoEvent
        ) nothing
