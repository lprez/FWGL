{-# LANGUAGE DataKinds, FlexibleContexts, ConstraintKinds, TypeOperators,
             TypeFamilies #-}

{-| Simplified 3D graphics system. -}
module FWGL.Graphics.D3 (
        -- * Elements
        Element,
        cube,
        -- ** Geometry
        Geometry,
        Geometry3,
        geom,
        mkGeometry3,
        -- * Textures
        module FWGL.Graphics.Color,
        Texture,
        textureURL,
        textureFile,
        C.colorTex,
        mkTexture,
        -- * Transformations
        V3(..),
        pos,
        rotX,
        rotY,
        rotZ,
        rotAA,
        scale,
        scaleV,
        transform,
        -- * Layers
        Layer,
        -- ** Element layers
        elements,
        view,
        -- ** Object layers
        Program,
        layer,
        layerPrg,
        program, -- TODO: wrong!
        -- ** Sublayers
        C.subLayer,
        -- * Custom 3D objects
        Object,
        object,
        object1,
        (C.~~),
        -- ** Globals
        C.global,
        C.globalTexture,
        C.globalTexSize,
        viewObject,
        DefaultUniforms3D,
        Texture2(..),
        Transform3(..),
        View3(..),
        -- * 3D matrices
        V4(..),
        M4(..),
        mat4,
        mul4,
        -- ** View matrices
        perspectiveMat4,
        cameraMat4,
        -- ** Transformation matrices
        idMat4,
        transMat4,
        rotXMat4,
        rotYMat4,
        rotZMat4,
        rotAAMat4,
        scaleMat4
) where

import Control.Applicative
import FWGL.Backend hiding (Texture, Program)
import FWGL.Geometry
import qualified FWGL.Graphics.Custom as C
import FWGL.Graphics.Color
import FWGL.Graphics.Draw
import FWGL.Graphics.Shapes
import FWGL.Graphics.Types
import FWGL.Internal.TList
import FWGL.Shader.Default3D (Texture2, Transform3, View3)
import FWGL.Shader.Program hiding (program)
import FWGL.Graphics.Texture
import FWGL.Vector

-- | A 3D object with a 'Texture' and a transformation.
data Element = Element Texture (Draw M4) (Geometry Geometry3)

-- | A cube with a specified 'Texture'.
cube :: GLES => Texture -> Element
cube t = Element t (return idMat4) cubeGeometry

-- | An element with a specified 'Geometry' and 'Texture'.
geom :: Texture -> Geometry Geometry3 -> Element
geom t = Element t $ return idMat4

-- | Create a graphical 'Object' from a list of 'Element's and a view matrix.
object :: BackendIO => M4 -> [Element] -> Object DefaultUniforms3D Geometry3
object m = viewObject m . foldl acc ObjectEmpty
        where acc o e = o C.~~ object1 e

-- | Create a graphical 'Object' from a single 'Element'. This lets you set your
-- own globals individually. If the shader uses the view matrix 'View3' (e.g.
-- the default 3D shader), you have to set it with 'viewObject'.
object1 :: BackendIO => Element -> Object '[Transform3, Texture2] Geometry3
object1 (Element t m g) = C.globalDraw (undefined :: Transform3) m $
                          C.globalTexture (undefined :: Texture2) t $
                          C.static g

-- | Create a standard 'Layer' from a list of 'Element's.
elements :: BackendIO => [Element] -> Layer
elements = layer . object idMat4

-- | Create a 'Layer' from a view matrix and a list of 'Element's.
view :: BackendIO => M4 -> [Element] -> Layer
view m = layer . object m

-- | Set the value of the view matrix of a 3D 'Object'.
viewObject :: BackendIO => M4 -> Object gs Geometry3
           -> Object (View3 ': gs) Geometry3
viewObject = C.global (undefined :: View3)

-- | Create a 'Layer' from a 3D 'Object', using the default shader.
layer :: BackendIO => Object DefaultUniforms3D Geometry3 -> Layer
layer = layerPrg defaultProgram3D

-- | Create a 'Layer' from a 3D 'Object', using a custom shader.
layerPrg :: (BackendIO, Subset og pg) => Program pg Geometry3
         -> Object og Geometry3 -> Layer
layerPrg = C.layer

-- | Translate an 'Element'.
pos :: V3 -> Element -> Element
pos v = transform $ transMat4 v

-- | Rotate an 'Element' around the X axis.
rotX :: Float -> Element -> Element
rotX a = transform $ rotXMat4 a

-- | Rotate an 'Element' around the X axis.
rotY :: Float -> Element -> Element
rotY a = transform $ rotYMat4 a

-- | Rotate an 'Element' around the X axis.
rotZ :: Float -> Element -> Element
rotZ a = transform $ rotZMat4 a

-- | Rotate an 'Element' around an axis and an angle.
rotAA :: V3 -> Float -> Element -> Element
rotAA ax ag = transform $ rotAAMat4 ax ag

-- | Scale an 'Element'.
scale :: Float -> Element -> Element
scale f = transform $ scaleMat4 (V3 f f f)

-- | Scale an 'Element' in three dimensions.
scaleV :: V3 -> Element -> Element
scaleV v = transform $ scaleMat4 v

-- | Transform an 'Element'.
transform :: M4 -> Element -> Element
transform m' (Element t m g) = Element t (mul4 <$> m <*> pure m') g
