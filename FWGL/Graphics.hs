{-# LANGUAGE DataKinds, FlexibleContexts, ConstraintKinds, TypeOperators,
             TypeFamilies #-}

module FWGL.Graphics (
        module FWGL.Graphics.Color,
        Scene,
        Object,
        Geometry,
        Texture,
        Color(..),
        V2(..),
        V3(..),
        (~~),
        (<>),
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
        rotAA,
        scale,
        scaleV,
        color,
        texture,
        dynamicE,
        dynamic,
        scene,
        transform
) where

import FRP.Yampa
import FWGL.Geometry
import FWGL.Graphics.Color
import FWGL.Graphics.Types
import FWGL.Internal.GL (GLES)
import FWGL.Internal.TList
import FWGL.Shader.Program
import FWGL.Texture
import FWGL.Vector

-- | Transformed objects are those that support the uniforms of the standard
-- shaders/program (i.e. Transform3 and Texture2, but not View3)
type TransformedObject = Object '[Transform3, Texture2] DefaultAttributes

-- | An empty transformed object.
nothing :: TransformedObject
nothing = ObjectEmpty

-- | An empty custom object. You should use a custom object if your shader is
-- not based on the two standard uniforms.
cnothing :: Object '[] '[]
cnothing = ObjectEmpty

-- | A transformed cube.
cube :: GLES => Texture -> Transformation -> TransformedObject
cube tx tr = transform tr . texture tx $
                (ObjectMesh Cube :: Object '[] Geometry3)

-- | A custom cube.
ccube :: Object is Geometry3
ccube = ObjectMesh Cube

-- | Join two objects.
(~~) :: (Equal gs gs', Equal is is')
     => Object gs is -> Object gs' is'
     -> Object (Union gs gs') (Union is is')
(~~) = ObjectAppend

-- | Join two objects, even if they don't provide the same variables.
unsafeJoin :: Object gs is -> Object gs' is'
           -> Object (Union gs gs') (Union is is')
unsafeJoin = ObjectAppend

-- | A transformed object with a specified 'Geometry'.
geom :: GLES => Texture -> Transformation -> Geometry i
     -> Object '[Transform3, Texture2] i
geom tx tr = transform tr . texture tx . ObjectMesh . StaticGeom

-- | A custom object with a specified 'Geometry'
cgeom :: Geometry i -> Object g i
cgeom = ObjectMesh . StaticGeom

-- | A transformation that does nothing.
same :: Transformation
same = Transformation idMat

-- | Translate an object.
translate :: V3 -> Transformation
translate = Transformation . transMat

-- | Rotate around the X axis.
rotX :: Float -> Transformation
rotX = Transformation . rotXMat

-- | Rotate around the Y axis.
rotY :: Float -> Transformation
rotY = Transformation . rotYMat

-- | Rotate around the Z axis.
rotZ :: Float -> Transformation
rotZ = Transformation . rotZMat

-- | Rotate using a rotation axis and angle.
rotAA :: V3 -> Float -> Transformation
rotAA v = Transformation . rotAAMat v

-- | Scale an object.
scale :: Float -> Transformation
scale f = Transformation $ scaleMat (V3 f f f)

-- | Scale in three dimensions.
scaleV :: V3 -> Transformation
scaleV = Transformation . scaleMat

-- | Generate a 1x1 texture.
color :: Color -> Texture
color c = mkTexture 1 1 [ c ]

-- | Set the texture of a custom object.
texture :: NotMember Texture2 g => Texture
        -> Object g i -> Object (Texture2 ': g) i
texture = ObjectTexture

-- | Create a standard scene (a scene with the standard shaders).
scene :: ( Subset gs DefaultUniforms
         , Equal '[Transform3, Texture2] gs
         , Equal DefaultAttributes is, GLES)
      => Object gs is -> Scene
scene = sceneM4 idMat

-- | Create a standard scene with perspective.
scenePersp :: ( Subset gs DefaultUniforms
              , Equal '[Transform3, Texture2] gs
              , Equal DefaultAttributes is, GLES)
            => Object gs is -> Scene
scenePersp = undefined -- TODO

-- | Create a standard scene with a view matrix
sceneM4 :: ( Subset gs DefaultUniforms
           , Equal gs '[Transform3, Texture2]
           , Equal DefaultAttributes is, GLES )
        => M4 -> Object gs is -> Scene
sceneM4 m = Scene defaultProgram . ObjectGlobal (undefined :: View3) m

-- sceneProgram 

-- | Like 'dynamic', but instead of comparing the two objects it checks the
-- event with the new object.
dynamicE :: Object g i -- ^ Initial 'Object'.
         -> SF (Event (Object g i)) (Object g i)
dynamicE = dynamicG $ flip const

-- | Automatically deallocate the previous mesh from the GPU when it changes.
dynamic :: SF (Object g i) (Object g i)
dynamic = undefined
{-
dynamic =
        dynamicG (\ o n -> if objectGeometry o == objectGeometry n
                                then Event n
                                else NoEvent
        ) nothing
-}

dynamicG :: (Object g i -> a -> Event (Object g i)) -> (Object g i) -> SF a (Object g i)
dynamicG = undefined
{-
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
-}

-- | Transform a custom object.
transform :: (NotMember Transform3 g, GLES) => Transformation
          -> Object g i -> Object (Transform3 ': g) i
transform (Transformation m) = ObjectGlobal (undefined :: Transform3) m
