{-# LANGUAGE GADTs, TypeOperators, KindSignatures, DataKinds,
             MultiParamTypeClasses #-}

module FWGL.Geometry (
        AttrList(..),
        Geometry(..),
        Geometry3,
        GPUGeometry(..),
        mkGeometry,
        mkGeometry3,
        castGeometry,
        facesToArrays,
        arraysToElements,
        triangulate
) where

import Control.Applicative
import Control.Monad.ST
import qualified Data.Hashable as H
import qualified Data.HashMap.Strict as H
import Data.Foldable (Foldable, forM_)
import Data.STRef
import qualified Data.Vector.Storable as V
import Data.Word (Word16, Word)
import Unsafe.Coerce

import FWGL.Internal.GL
import FWGL.Internal.Resource
import FWGL.Shader.CPU
import FWGL.Shader.Default (Position3, Normal3, UV)
import FWGL.Shader.GLSL (attributeName)
import FWGL.Vector

data AttrList (is :: [*]) where
        AttrListNil :: AttrList '[]
        AttrListCons :: (H.Hashable c, AttributeCPU c g)
                     => g -> [c] -> AttrList gs -> AttrList (g ': gs)

data Geometry (is :: [*]) = Geometry {
        attributes :: AttrList is,
        elements :: [Word16],
        hash :: Int
}

data GPUGeometry = GPUGeometry {
        attributeBuffers :: [(String, Buffer, GLUInt -> GL ())],
        elementBuffer :: Buffer,
        elementCount :: Int
}

type Geometry3 = '[Position3, UV, Normal3]

instance H.Hashable (AttrList is) where
        hashWithSalt salt AttrListNil = salt
        hashWithSalt salt (AttrListCons _ i is) = H.hashWithSalt salt (i, is)

instance H.Hashable (Geometry is) where
        hashWithSalt salt g = H.hashWithSalt salt $ hash g

instance Eq (Geometry is) where
        g == g' = hash g == hash g'

-- | Create a 3D 'Geometry'. The first three lists should have the same length.
mkGeometry3 :: GLES
            => [V3]     -- ^ List of vertices.
            -> [V2]     -- ^ List of UV coordinates.
            -> [V3]     -- ^ List of normals.
            -> [Word16] -- ^ Triangles expressed as triples of indices to the
                       --   three lists above.
            -> Geometry Geometry3
mkGeometry3 v u n = mkGeometry (AttrListCons (undefined :: Position3) v $
                                AttrListCons (undefined :: UV) u $
                                AttrListCons (undefined :: Normal3) n
                                AttrListNil)

-- | Create a custom 'Geometry'.
mkGeometry :: GLES => AttrList is -> [Word16] -> Geometry is
mkGeometry al e = Geometry al e $ H.hash al

castGeometry :: Geometry is -> Geometry is'
castGeometry = unsafeCoerce

instance GLES => Resource (Geometry i) GPUGeometry GL where
        loadResource = loadGeometry
        unloadResource _ = deleteGPUGeometry

loadGeometry :: GLES => Geometry i -> GL GPUGeometry
loadGeometry (Geometry al es _) =
        GPUGeometry <$> loadAttrList al
                    <*> (liftIO (encodeUShorts es) >>=
                            loadBuffer gl_ELEMENT_ARRAY_BUFFER)
                    <*> pure (length es)

loadAttrList :: GLES => AttrList is -> GL [(String, Buffer, GLUInt -> GL ())]
loadAttrList AttrListNil = return []
loadAttrList (AttrListCons g c al) = (:) <$> loadAttribute g c
                                         <*> loadAttrList al
        where loadAttribute g c = do arr <- encodeAttribute g c
                                     buf <- loadBuffer gl_ARRAY_BUFFER arr
                                     return (attributeName g, buf, setAttribute g)

deleteGPUGeometry :: GLES => GPUGeometry -> GL ()
deleteGPUGeometry (GPUGeometry abs eb _) = mapM_ (\(_, buf, _) -> deleteBuffer buf) abs
                                           >> deleteBuffer eb

-- TODO: move
loadBuffer :: GLES => GLEnum -> Array -> GL Buffer
loadBuffer ty bufData =
        do buffer <- createBuffer
           bindBuffer ty buffer
           bufferData ty bufData gl_STATIC_DRAW
           bindBuffer ty noBuffer
           return buffer


-- TODO: use dlist
arraysToElements :: (Foldable f, GLES) => f (V3, V2, V3) -> Geometry Geometry3
arraysToElements arrays = runST $
        do vs <- newSTRef []
           us <- newSTRef []
           ns <- newSTRef []
           es <- newSTRef []
           triples <- newSTRef H.empty
           len <- newSTRef 0

           forM_ arrays $ \ t@(v, u, n) -> readSTRef triples >>= \ ts ->
                   case H.lookup t ts of
                           Just idx -> modifySTRef es (idx :)
                           Nothing -> do idx <- readSTRef len
                                         writeSTRef len $ idx + 1
                                         writeSTRef triples $ H.insert t idx ts
                                         modifySTRef vs (v :)
                                         modifySTRef us (u :)
                                         modifySTRef ns (n :)
                                         modifySTRef es (idx :)

           mkGeometry3 <$> (reverse <$> readSTRef vs)
                       <*> (reverse <$> readSTRef us)
                       <*> (reverse <$> readSTRef ns)
                       <*> (reverse <$> readSTRef es)

facesToArrays :: V.Vector V3 -> V.Vector V2 -> V.Vector V3
              -> [[(Int, Int, Int)]] -> [(V3, V2, V3)]
facesToArrays ovs ous ons = (>>= toIndex . triangulate)
        where toIndex = (>>= \(v1, v2, v3) -> [ getVertex v1
                                              , getVertex v2
                                              , getVertex v3 ])
              getVertex (v, u, n) = (ovs V.! v, ous V.! u, ons V.! n)

triangulate :: [a] -> [(a, a, a)]
triangulate [] = error "triangulate: empty face"
triangulate (_ : []) = error "triangulate: can't triangulate a point"
triangulate (_ : _ : []) = error "triangulate: can't triangulate an edge"
triangulate (x : y : z : []) = [(x, y, z)]
triangulate (x : y : z : w : []) = [(x, y, z), (x, z, w)]
triangulate _ = error "triangulate: can't triangulate >4 faces"
