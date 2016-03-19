module FWGL.Model (
        Model(..),
        facesToArrays,
        arraysToModel
) where

import Control.Applicative
import Control.Monad.ST
import Data.Foldable (Foldable, forM_)
import Data.STRef

-- | A model format suitable to be used with OpenGL drawElements.
data Model = Model {
        modelVertices :: [(Float, Float, Float)],
        modelUVCoords :: [(Float, Float)],
        modelNormals :: [(Float, Float, Float)],
        modelElements :: [Word16]
} deriving (Show)

arraysToModel :: Foldable f
              => f ( (Float, Float, Float)
                   , (Float, Float)
                   , (Float, Float, Float) )
              -> Model
arraysToModel arr = let (v, u, n, e) = arraysToElements arr
                    in Model v u n e

arraysToElements :: Foldable f
                 => f ( (Float, Float, Float)
                      , (Float, Float)
                      , (Float, Float, Float) )
                 -> ( [(Float, Float, Float)]
                    , [(Float, Float)]
                    , [(Float, Float, Float)]
                    , [Word16])
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

           (,,,) <$> (reverse <$> readSTRef vs)
                 <*> (reverse <$> readSTRef us)
                 <*> (reverse <$> readSTRef ns)
                 <*> (reverse <$> readSTRef es)

facesToArrays :: V.Vector (Float, Float, Float)
              -> V.Vector (Float, Float)
              -> V.Vector (Float, Float, Float)
              -> [[(Int, Int, Int)]]
              -> [( (Float, Float, Float)
                  , (Float, Float)
                  , (Float, Float, Float) )]
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
