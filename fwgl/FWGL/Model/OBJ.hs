module FWGL.Model.OBJ where

import Control.Applicative
import Control.Monad (when)
import Control.Monad.ST
import qualified Data.Vector.Storable as V
import Data.STRef
import Data.Word (Word16)

import FWGL.Model
import FWGL.Internal.STVectorLen

data OBJModel = OBJModel {
        objVertices :: V.Vector (Float, Float, Float),
        objUVCoords :: V.Vector (Float, Float),
        objNormals :: V.Vector (Float, Float, Float),
        objFaces :: [[(Int, Int, Int)]]
} deriving (Show)

-- | Parse an .obj file.
parseOBJ :: String -> OBJModel
parseOBJ file = runST $
        do vs <- new
           us <- new
           ns <- new
           fs <- newSTRef []

           flip mapM_ (lines file) $ \ l -> case l of
                   ('v' : ' ' : v3) -> cons (parseVec3 v3) vs
                   ('v' : 't' : ' ' : v2) -> cons (parseVec2 v2) us
                   ('v' : 'n' : ' ' : v3) -> cons (parseVec3 v3) ns
                   ('f' : ' ' : f) -> modifySTRef fs (parseFace f :)
                   _ -> return ()

           usLen <- readSTRef $ snd us
           nsLen <- readSTRef $ snd ns

           when (usLen <= 0) $ cons (0, 0) us
           when (nsLen <= 0) $ cons (0, 0, 0) ns

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
              parseVec3 str = case split ' ' '#' str of
                                (z : y : x : _) -> ( parseFloat x ) 
                                                   , parseFloat y )
                                                   , parseFloat z )
                                _ -> error "parseOBJ: invalid vertex/normal"
              parseVec2 str = case split ' ' '#' str of
                                (y : x : _) -> ( parseFloat x )
                                               , parseFloat y )
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

-- | Convert an 'OBJModel' to a 'Model'.
attributesOBJ :: OBJModel -> Model
attributesOBJ (OBJModel v u n fs) = arraysToModel $ facesToArrays v u n fs
