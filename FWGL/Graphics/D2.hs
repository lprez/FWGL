{-# LANGUAGE DataKinds, FlexibleContexts, ConstraintKinds, TypeOperators,
             TypeFamilies #-}

module FWGL.Graphics.D2 (
        module FWGL.Graphics.Color,
        Element(..),
        Object,
        Layer,
        V2(..),
        (C.~~),
        rect,
        geom,
        image,
        depth,
        sprite,
        object,
        elements,
        view,
        viewObject,
        pos,
        rot,
        scale,
        scaleV,
        C.global,
        mkGeometry2,
        mkTexture,
        C.textureURL,
        C.colorTex
) where

import Control.Applicative
import FWGL.Backend hiding (Texture, Image)
import FWGL.Geometry
import qualified FWGL.Graphics.Custom as C
import FWGL.Graphics.Color
import FWGL.Graphics.Draw
import FWGL.Graphics.Shapes
import FWGL.Graphics.Types
import FWGL.Shader.Default2D (Image, Depth, Transform2, View2)
import FWGL.Shader.Program
import FWGL.Texture
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
image :: BackendIO      -- ^ This is required because it needs to know how to
                        -- get the size of the 'Texture'.
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
object m = C.global (undefined :: View2) m . foldl acc ObjectEmpty
        where acc o e = o C.~~ obj1 e
              obj1 (Element d t m g) =
                      C.globalTexture (undefined :: Image) t $
                      C.global (undefined :: Depth) d $
                      C.globalDraw (undefined :: Transform2) m $
                      C.static g

-- | Create a standard 'Layer' from a list of 'Element's.
elements :: BackendIO => [Element] -> Layer
elements = viewObject . object idMat3

-- | Create a 'Layer' from a view matrix and a list of 'Element's.
view :: BackendIO => M3 -> [Element] -> Layer
view m = viewObject . object m

-- | Create a 'Layer' from a 2D 'Object'.
viewObject :: BackendIO => Object DefaultUniforms2D Geometry2 -> Layer
viewObject = C.layer defaultProgram2D

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
