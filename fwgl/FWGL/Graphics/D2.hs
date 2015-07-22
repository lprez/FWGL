{-# LANGUAGE DataKinds, FlexibleContexts, ConstraintKinds, TypeOperators,
             TypeFamilies #-}

{-| Simplified 2D graphics system. -}
module FWGL.Graphics.D2 (
        -- * Elements
        Element,
        rect,
        image,
        depth,
        sprite,
        -- ** Geometry
        Geometry,
        Geometry2,
        geom,
        mkGeometry2,
        -- * Textures
        module FWGL.Graphics.Color,
        Texture,
        C.textureURL,
        C.textureFile,
        C.colorTex,
        mkTexture,
        -- * Transformations
        V2(..),
        pos,
        rot,
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
        C.program,
        -- ** Sublayers
        C.subLayer,
        C.depthSubLayer,
        -- * Custom 2D objects
        Object,
        object,
        object1,
        (C.~~),
        -- ** Globals
        C.global,
        C.globalTexture,
        C.globalTexSize,
        viewObject,
        DefaultUniforms2D,
        Image(..),
        Depth(..),
        Transform2(..),
        View2(..),
        -- * 3D matrices
        V3(..),
        M3(..),
        mat3,
        mul3,
        -- ** Transformation matrices
        idMat3,
        transMat3,
        rotMat3,
        scaleMat3
) where

import Control.Applicative
import FWGL.Backend hiding (Texture, Image, Program)
import FWGL.Geometry
import qualified FWGL.Graphics.Custom as C
import FWGL.Graphics.Color
import FWGL.Graphics.Draw
import FWGL.Graphics.Shapes
import FWGL.Graphics.Types hiding (program)
import FWGL.Graphics.Texture
import FWGL.Internal.TList
import FWGL.Shader.Default2D (Image, Depth, Transform2, View2)
import FWGL.Shader.Program
import FWGL.Vector

-- | A 2D object with a 'Texture', a depth and a transformation.
data Element = Element Float Texture (Draw M3) (Geometry Geometry2)

-- | A rectangle with a specified 'Texture' and size.
rect :: GLES => V2 -> Texture -> Element
rect v t = Element 0 t (return idMat3) $ rectGeometry v

-- | An element with a specified 'Geometry' and 'Texture'.
geom :: Texture -> Geometry Geometry2 -> Element
geom t = Element 0 t $ return idMat3

-- | A rectangle with the aspect ratio adapted to its texture.
image :: BackendIO
      => Float          -- ^ Width.
      -> Texture -> Element
image s t = Element 0 t ((\(w, h) -> scaleMat3 (V2 1 $ h /w)) <$> textureSize t)
                        (rectGeometry $ V2 s s)

-- | Set the depth of an element.
depth :: Float -> Element -> Element
depth d (Element _ t m g) = Element d t m g

-- | A rectangle with the size and aspect ratio adapted to the screen. You
-- have to use the 'FWGL.Utils.screenScale' view matrix.
sprite :: BackendIO => Texture -> Element
sprite t = Element 0 t ((\(w, h) -> scaleMat3 $ V2 w h) <$> textureSize t)
                       (rectGeometry $ V2 1 1)

-- | Create a graphical 'Object' from a list of 'Element's and a view matrix.
object :: BackendIO => M3 -> [Element] -> Object DefaultUniforms2D Geometry2
object m = viewObject m . foldl acc ObjectEmpty
        where acc o e = o C.~~ object1 e

-- | Create a graphical 'Object' from a single 'Element'. This lets you set your
-- own globals individually. If the shader uses the view matrix 'View2' (e.g.
-- the default 2D shader), you have to set it with 'viewObject'.
object1 :: BackendIO => Element -> Object '[Image, Depth, Transform2] Geometry2
object1 (Element d t m g) = C.globalTexture (undefined :: Image) t $

-- TODO: object1Image, object1Depth, object1Trans, object1ImageDepth,
-- object1ImageTrans, object1DepthTrans
                            C.global (undefined :: Depth) d $
                            C.globalDraw (undefined :: Transform2) m $
                            C.static g

-- | Create a standard 'Layer' from a list of 'Element's.
elements :: BackendIO => [Element] -> Layer
elements = layer . object idMat3

-- | Create a 'Layer' from a view matrix and a list of 'Element's.
view :: BackendIO => M3 -> [Element] -> Layer
view m = layer . object m

-- | Set the value of the view matrix of a 2D 'Object'.
viewObject :: BackendIO => M3 -> Object gs Geometry2
           -> Object (View2 ': gs) Geometry2
viewObject = C.global (undefined :: View2)

-- | Create a 'Layer' from a 2D 'Object', using the default shader.
layer :: BackendIO => Object DefaultUniforms2D Geometry2 -> Layer
layer = layerPrg defaultProgram2D

-- | Create a 'Layer' from a 2D 'Object', using a custom shader.
layerPrg :: (BackendIO, Subset og pg) => Program pg Geometry2
         -> Object og Geometry2 -> Layer
layerPrg = C.layer

-- | Translate an 'Element'.
pos :: V2 -> Element -> Element
pos v = transform $ transMat3 v

-- | Rotate an 'Element'.
rot :: Float -> Element -> Element
rot a = transform $ rotMat3 a

-- | Scale an 'Element'.
scale :: Float -> Element -> Element
scale f = transform $ scaleMat3 (V2 f f)

-- | Scale an 'Element' in two dimensions.
scaleV :: V2 -> Element -> Element
scaleV v = transform $ scaleMat3 v

-- | Transform an 'Element'.
transform :: M3 -> Element -> Element
transform m' (Element d t m g) = Element d t (mul3 <$> m <*> pure m') g
