module FWGL.Geometry.OBJ where

import Control.Applicative
import Control.Monad (when)
import Control.Monad.ST
import qualified Data.Vector.Storable as V
import Data.STRef
import Data.Word (Word16)

import FWGL.Backend (GLES)
import FWGL.Internal.STVectorLen
import FWGL.Geometry
import FWGL.Vector

data OBJModel = OBJModel {
        objVertices :: V.Vector V3,
        objUVCoords :: V.Vector V2,
        objNormals :: V.Vector V3,
        objFaces :: [[(Int, Int, Int)]]
} deriving (Show)

parseOBJ :: String -> OBJModel
parseOBJ file = runST $
        do vs <- new
           us <- new
           ns <- new
           fs <- newSTRef []

           flip mapM_ (lines file) $ \ l -> case l of
                   ('v' : ' ' : v3) -> cons (parseV3 v3) vs
                   ('v' : 't' : ' ' : v2) -> cons (parseV2 v2) us
                   ('v' : 'n' : ' ' : v3) -> cons (parseV3 v3) ns
                   ('f' : ' ' : f) -> modifySTRef fs (parseFace f :)
                   _ -> return ()

           usLen <- readSTRef $ snd us
           nsLen <- readSTRef $ snd ns

           when (usLen <= 0) $ cons (V2 0 0) us
           when (nsLen <= 0) $ cons (V3 0 0 0) ns

           OBJModel <$> freeze vs
                    <*> freeze us
                    <*> freeze ns
                    <*> readSTRef fs

        where split s e str = iter str "" []
                where iter (x : xs) cur fnd | x == s = iter xs "" $ 
                                                           reverse cur : fnd
                                            | x /= e = iter xs (x : cur) fnd
                      iter _ [] fnd = fnd
                      iter _ cur fnd = reverse cur : fnd
              parseV3 str = case split ' ' '#' str of
                                (z : y : x : _) -> V3 (parseFloat x) 
                                                      (parseFloat y)
                                                      (parseFloat z)
                                _ -> error "parseOBJ: invalid vertex/normal"
              parseV2 str = case split ' ' '#' str of
                                (y : x : _) -> V2 (parseFloat x) (parseFloat y)
                                _ -> error "parseOBJ: invalid uv coordinate"
              parseFace = map parseElement . reverse . split ' ' '#'

              parseElement str = case split '/' ' ' str of
                                     (n : t : v : _) -> ( parseInt v - 1
                                                        , parseInt t - 1
                                                        , parseInt n - 1 )
                                     _ -> error "parseOBJ: invalid element"

              parseInt [] = 1
              parseInt s = read s

              parseFloat [] = 0
              parseFloat s = read s

attributesOBJ :: OBJModel -> ([V3], [V2], [V3], [Word16])
attributesOBJ (OBJModel v u n fs) = arraysToElements $ facesToArrays v u n fs

geometryOBJ :: GLES => OBJModel -> Geometry Geometry3
geometryOBJ o = let (v, u, n, e) = attributesOBJ o in mkGeometry3 v u n e
