module FWGL.Geometry (
        Geometry(..),
        mkGeometry,
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
import Data.Word (Word16)

import FWGL.Vector

data Geometry = Geometry {
        vertices :: [V3],
        uvCoords :: [V2],
        normals :: [V3],
        elements :: [Word16],
        hash :: Int
} deriving (Show)

instance H.Hashable Geometry where
        hashWithSalt salt g = H.hashWithSalt salt $ hash g

instance Eq Geometry where
        g == g' = hash g == hash g'

mkGeometry :: [V3] -> [V2] -> [V3] -> [Word16] -> Geometry
mkGeometry v u n e = Geometry { vertices = v
                              , uvCoords = u
                              , normals = n
                              , elements = e
                              , hash = H.hash (v, u, n, e) }

-- TODO: use dlist
arraysToElements :: Foldable f => f (V3, V2, V3) -> Geometry
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

           mkGeometry <$> (reverse <$> readSTRef vs)
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
