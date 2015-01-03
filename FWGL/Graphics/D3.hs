{-# LANGUAGE DataKinds, FlexibleContexts, ConstraintKinds, TypeOperators,
             TypeFamilies #-}

module FWGL.Graphics.D3 (
        module FWGL.Graphics.Color,
        Transformation(..),
        Layer,
        TransformedObject,
        Object3D,
        Geometry,
        Texture,
        Color(..),
        V2(..),
        V3(..),
        M4(..),
        (~~),
        (<>),
        mkGeometry3,
        mkTexture,
        textureURL,
        vec2,
        vec3,
        mat4,
        nothing,
        cube,
        geom,
        global,
        translate,
        rotX,
        rotY,
        rotZ,
        rotAA,
        scale,
        scaleV,
        colorTex,
        scene,
        sceneM4,
        assoc,
        transform
) where

import Data.Monoid
import FWGL.Backend (GLES, BackendIO)
import FWGL.Geometry
import FWGL.Graphics.Color
import FWGL.Graphics.Custom hiding (nothing, geom, scene)
import FWGL.Graphics.Types
import FWGL.Internal.TList
import FWGL.Shader.Default3D (Texture2, Transform3, View3)
import FWGL.Shader.Program
import FWGL.Texture
import FWGL.Vector

-- | A transformation matrix.
newtype Transformation = Transformation M4

instance Monoid Transformation where
        mempty = Transformation idMat
        mappend (Transformation t') (Transformation t) = Transformation $
                                                                mul4 t t'
-- | Transformed 3D objects are those that support the uniforms of the standard
-- shaders/program (i.e. Transform3 and Texture2, but not View3)
type TransformedObject = Object3D '[Transform3, Texture2]

-- | 3D objects are those that support the attributes of the standard 3d
-- shaders/program.
type Object3D gs = Object gs Geometry3

-- | An empty transformed object.
nothing :: TransformedObject
nothing = ObjectEmpty

-- | A transformed cube.
cube :: BackendIO => Texture -> Transformation -> TransformedObject
cube tx tr = transform tr . texture tx $
                (ObjectMesh Cube :: Object '[] Geometry3)

-- | A transformed object with a specified 'Geometry'.
geom :: BackendIO => Texture -> Transformation -> Geometry Geometry3
     -> TransformedObject
geom tx tr = transform tr . texture tx . ObjectMesh . StaticGeom

-- | Set the texture of an object.
texture :: (NotMember Texture2 g, BackendIO) => Texture
        -> Object g i -> Object (Texture2 ': g) i
texture = globalTexture (undefined :: Texture2)

-- | A transformation that does nothing.
same :: Transformation
same = Transformation idMat

-- | Translate.
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

-- | Create a standard scene (a 'Layer' with the standard shaders).
scene :: (Subset gs DefaultUniforms3D, Equal '[Transform3, Texture2] gs, GLES)
      => Object3D gs -> Layer
scene = sceneM4 idMat

-- sceneView :: View -> ...
-- view :: View -> Object ... -> Object ...
-- assocView :: View ->
-- perspective ::  ... -> View
-- lookAt :: ... -> View
-- layer ...

-- | Create a standard scene with a view matrix
sceneM4 :: (Subset gs DefaultUniforms3D, Equal gs '[Transform3, Texture2], GLES)
        => M4 -> Object3D gs -> Layer
sceneM4 m = assoc defaultProgram3D . global (undefined :: View3) m

-- | Transform an object.
transform :: (NotMember Transform3 g, GLES) => Transformation
          -> Object g i -> Object (Transform3 ': g) i
transform (Transformation m) = global (undefined :: Transform3) m
