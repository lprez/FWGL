module FWGL.Graphics (
        module FWGL.Graphics.Color,
        Scene,
        Object,
        Geometry,
        Texture,
        Color(..),
        V2(..),
        V3(..),
        mkGeometry,
        mkTexture,
        textureURL,
        vec2,
        vec3,
        nothing,
        cube,
        geom,
        translate,
        rotX,
        rotY,
        rotZ,
        scale,
        color,
        texture,
        dynamicE,
        dynamic
) where

import FRP.Yampa
import FWGL.Geometry (mkGeometry)
import FWGL.Graphics.Color
import FWGL.Graphics.Types
import FWGL.Texture
import FWGL.Vector

-- | An empty object.
nothing :: Object
nothing = SolidObject $ Solid Empty idMat whiteTexture

-- | A cube.
cube :: Float  -- ^ Edge
     -> Object
cube edg = scale edg . SolidObject $ Solid Cube idMat whiteTexture

-- | An object with a specified 'Geometry'.
geom :: Geometry -> Object
geom g = SolidObject $ Solid (StaticGeom g) idMat whiteTexture

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

-- | Set the color of an object.
color :: Color -> Object -> Object
color c = texture $ mkTexture 1 1 [ c ]

-- | Set the texture of an object.
texture :: Texture -> Object -> Object
texture t (SolidObject (Solid mesh mat _)) = SolidObject $ Solid mesh mat t
texture _ o = o

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

transform :: M4 -> Object -> Object
transform mat' (SolidObject (Solid mesh mat t)) =
                SolidObject $ Solid mesh (mul4 mat' mat) t -- TODO
transform _ o = o

dynamicG :: (Object -> a -> Event Object) -> Object -> SF a Object
dynamicG f i = flip sscan i $ \ oldObj inp ->
                case f oldObj inp of
                        Event (SolidObject (Solid (StaticGeom newG) mat tex)) ->
                                SolidObject (
                                        Solid (DynamicGeom (objectGeometry oldObj)
                                                           newG)
                                              mat
                                              tex
                                )
                        NoEvent -> case oldObj of
                                        SolidObject (
                                                Solid (DynamicGeom _ new) mat t
                                                ) -> SolidObject (
                                                        Solid (StaticGeom new)
                                                              mat
                                                              t
                                                        )
                                        _ -> oldObj
                        _ -> error "dynamicG: not a Geometry."

whiteTexture :: Texture
whiteTexture = mkTexture 1 1 [ white ]
