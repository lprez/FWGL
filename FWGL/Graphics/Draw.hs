{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module FWGL.Graphics.Draw (
        Draw,
        DrawState,
        execDraw,
        drawInit,
        drawBegin,
        drawScene,
        resize
) where

import FWGL.Graphics.Color
import FWGL.Graphics.Shapes
import FWGL.Graphics.Types
import FWGL.Texture
import FWGL.Vector

import JavaScript.WebGL hiding (getCtx, Texture)
import qualified JavaScript.WebGL as WebGL

import Data.Hashable (Hashable)
import qualified Data.HashMap.Strict as H
import Data.Word (Word, Word16)
import Control.Applicative
import Control.Monad.IO.Class
import Control.Monad.Trans.State
import GHCJS.Foreign
import GHCJS.Marshal
import GHCJS.Types

data DrawState = DrawState {
        context :: Ctx,
        cubeBuffers :: GPUMesh, -- TODO: subst gpuMeshes
        modelUniform :: UniformLocation,
        samplerUniform :: UniformLocation,
        gpuMeshes :: H.HashMap Geometry GPUMesh,
        textures :: H.HashMap Texture WebGL.Texture
}

newtype Draw a = Draw { unDraw :: StateT DrawState IO a }
        deriving (Functor, Applicative, Monad, MonadIO)

data GPUMesh = GPUMesh {
        vertexBuffer :: Buffer,
        uvBuffer :: Buffer,
        normalBuffer :: Buffer,
        elementBuffer :: Buffer,
        elementCount :: Int
}

drawInit :: Ctx -> Int -> Int -> IO DrawState
drawInit ctx w h = do program <- loadShaders ctx defVS defFS
                      enable ctx gl_DEPTH_TEST
                      depthFunc ctx gl_LESS
                      clearColor ctx 0.0 0.0 0.0 1.0
                      resizeIO ctx w h
                      cube <- loadGeometry ctx cubeGeometry
                      modelUniform <- getUniformLocation ctx program
                                                $ toJSString "modelMatrix"
                      samplerUniform <- getUniformLocation ctx program
                                                $ toJSString "sampler"
                      return DrawState { context = ctx
                                       , cubeBuffers = cube
                                       , modelUniform = modelUniform
                                       , samplerUniform = samplerUniform
                                       , gpuMeshes = H.empty
                                       , textures = H.empty }

execDraw :: Draw () -> DrawState -> IO DrawState
execDraw (Draw a) = execStateT a

resize :: Int -> Int -> Draw ()
resize w h = getCtx >>= \ctx -> liftIO $ resizeIO ctx w h

resizeIO :: Ctx -> Int -> Int -> IO ()
resizeIO ctx w h = viewport ctx 0 0 w h

drawBegin :: Draw ()
drawBegin = getCtx >>= \ctx -> liftIO $ clear ctx gl_COLOR_BUFFER_BIT

drawScene :: Scene -> Draw ()
drawScene = mapM_ drawObject

drawObject :: Object -> Draw ()
drawObject (SolidObject s) = drawSolid s

drawSolid :: Solid -> Draw ()
drawSolid (Solid mesh trans tex) =
        do ctx <- getCtx
           modelUni <- Draw $ modelUniform <$> get
           texUni <- Draw $ samplerUniform <$> get
           wtex <- getTexture tex
           liftIO $ do matarr <- encodeM4 trans
                       uniformMatrix4fv ctx modelUni False matarr
                       activeTexture ctx gl_TEXTURE0
                       bindTexture ctx gl_TEXTURE_2D wtex
                       uniform1i ctx texUni 0
           drawMesh mesh

drawMesh :: Mesh -> Draw ()
drawMesh Empty = return ()
drawMesh Cube = Draw (cubeBuffers <$> get) >>= drawGPUMesh
drawMesh (StaticGeom g) = getGPUMesh g >>= drawGPUMesh
drawMesh (DynamicGeom d g) = disposeGeometry d >> drawMesh (StaticGeom g)

getResource :: (Eq r, Hashable r)
            => (DrawState -> H.HashMap r g)
            -> (H.HashMap r g -> DrawState -> DrawState)
            -> (Ctx -> r -> IO g)
            -> r
            -> Draw g
getResource getMap putMap load res =
        do map <- Draw $ getMap <$> get
           case H.lookup res map of
                Just gpuRes -> return gpuRes
                Nothing -> do ctx <- getCtx
                              gpuRes <- liftIO $ load ctx res
                              Draw . modify $ putMap (H.insert res gpuRes map)
                              return gpuRes

getGPUMesh :: Geometry -> Draw GPUMesh
getGPUMesh = getResource gpuMeshes (\ m s -> s { gpuMeshes = m}) loadGeometry

getTexture :: Texture -> Draw WebGL.Texture
getTexture = getResource textures (\ m s -> s { textures = m }) loadTexture

drawGPUMesh :: GPUMesh -> Draw ()
drawGPUMesh (GPUMesh vb ub nb eb ec) = getCtx >>= \ctx -> liftIO $
        do withVAA ctx posAttribLoc vb 3 $ withVAA ctx normalAttribLoc nb 3 $
                withVAA ctx uvAttribLoc ub 2 $ do
                        bindBuffer ctx gl_ELEMENT_ARRAY_BUFFER eb
                        drawElements ctx gl_TRIANGLES ec gl_UNSIGNED_SHORT 0
                        bindBuffer ctx gl_ELEMENT_ARRAY_BUFFER noBuffer
           bindBuffer ctx gl_ARRAY_BUFFER noBuffer

           where withVAA ctx loc buf n act =
                   do bindBuffer ctx gl_ARRAY_BUFFER buf
                      enableVertexAttribArray ctx loc
                      vertexAttribPointer ctx loc n gl_FLOAT False 0 0
                      act
                      disableVertexAttribArray ctx loc

getCtx :: Draw Ctx
getCtx = Draw $ context <$> get

loadGeometry :: Ctx -> Geometry -> IO GPUMesh
loadGeometry ctx (Geometry vs us ns es _) =
        GPUMesh <$> (viewV3 vs >>= loadBuffer ctx gl_ARRAY_BUFFER)
                <*> (viewV2 us >>= loadBuffer ctx gl_ARRAY_BUFFER)
                <*> (viewV3 ns >>= loadBuffer ctx gl_ARRAY_BUFFER)
                <*> (viewWord16 es >>= loadBuffer ctx gl_ELEMENT_ARRAY_BUFFER)
                <*> (pure $ length es)

disposeGeometry :: Geometry -> Draw ()
disposeGeometry g = do gpuMesh <- getGPUMesh g
                       ctx <- getCtx
                       liftIO $ deleteGPUMesh ctx gpuMesh
                       Draw . modify $ \ s -> s {
                                gpuMeshes = H.delete g $ gpuMeshes s
                       }

deleteGPUMesh :: Ctx -> GPUMesh -> IO ()
deleteGPUMesh ctx (GPUMesh vb ub nb eb _) = do deleteBuffer ctx vb
                                               deleteBuffer ctx ub
                                               deleteBuffer ctx nb
                                               deleteBuffer ctx eb

loadTexture :: Ctx -> Texture -> IO WebGL.Texture
loadTexture ctx tex =
        do t <- createTexture ctx
           case tex of
                   (TexturePixels ps w h _) -> setup ctx t $
                           do arr <- viewColor ps
                              texImage2DBuffer ctx gl_TEXTURE_2D 0
                                                   gl_RGBA
                                                   w h 0
                                                   gl_RGBA
                                                   gl_UNSIGNED_BYTE
                                                   arr
                   (TextureURL url _) -> loadImage url $ \ img -> setup ctx t $
                           texImage2DElement ctx gl_TEXTURE_2D 0
                                                 gl_RGBA gl_RGBA
                                                 gl_UNSIGNED_BYTE
                                                 img
           return t
        where setup ctx t act = do
                bindTexture ctx gl_TEXTURE_2D t
                act
                texParameteri ctx gl_TEXTURE_2D gl_TEXTURE_MAG_FILTER gl_LINEAR
                texParameteri ctx gl_TEXTURE_2D gl_TEXTURE_MIN_FILTER gl_LINEAR
                texParameteri ctx gl_TEXTURE_2D gl_TEXTURE_WRAP_S gl_REPEAT
                texParameteri ctx gl_TEXTURE_2D gl_TEXTURE_WRAP_T gl_REPEAT
                bindTexture ctx gl_TEXTURE_2D noTexture

foreign import javascript unsafe
        "var img = new Image();                 \
        \img.src = $1;                          \
        \img.onload = function() { $2(img); };  "
        loadImageRaw :: JSString -> JSFun (JSRef a -> IO ()) -> IO ()

loadImage :: String -> (JSRef a -> IO ()) -> IO ()
loadImage url cb = asyncCallback1 AlwaysRetain cb
                   >>= loadImageRaw (toJSString url)

encodeM4 :: M4 -> IO Float32Array
encodeM4 (M4 (V4 a1 a2 a3 a4)
             (V4 b1 b2 b3 b4)
             (V4 c1 c2 c3 c4)
             (V4 d1 d2 d3 d4) ) = listToJSArray [ a1, a2, a3, a4
                                                , b1, b2, b3, b4
                                                , c1, c2, c3, c4
                                                , d1, d2, d3, d4 ]
                                  >>= float32Array

viewColor :: [Color] -> IO ArrayBufferView
viewColor v = toJSArray next (0, v) >>= uint8View
        where next (0, xs@(Color x _ _ _ : _)) = Just (x, (1, xs))
              next (1, xs@(Color _ y _ _ : _)) = Just (y, (2, xs))
              next (2, xs@(Color _ _ z _ : _)) = Just (z, (3, xs))
              next (3, Color _ _ _ w : xs) = Just (w, (0, xs))
              next (_, []) = Nothing

viewV3 :: [V3] -> IO ArrayBufferView
viewV3 v = toJSArray next (0, v) >>= float32View
        where next (0, xs@(V3 x _ _ : _)) = Just (x, (1, xs))
              next (1, xs@(V3 _ y _ : _)) = Just (y, (2, xs))
              next (2, V3 _ _ z : xs) = Just (z, (0, xs))
              next (_, []) = Nothing

viewV2 :: [V2] -> IO ArrayBufferView
viewV2 v = toJSArray next (False, v) >>= float32View
        where next (False, xs@(V2 x _ : _)) = Just (x, (True, xs))
              next (True, V2 _ y : xs) = Just (y, (False, xs))
              next (_, []) = Nothing

viewWord16 :: [Word16] -> IO ArrayBufferView
viewWord16 v = listToJSArray v >>= uint16View

toJSArray :: ToJSRef a => (v -> Maybe (a, v)) -> v -> IO (JSArray a)
toJSArray next iv = newArray >>= iterPush iv
        where iterPush v arr = case next v of
                                        Just (x, v') -> do xRef <- toJSRef x
                                                           pushArray xRef arr
                                                           iterPush v' arr
                                        Nothing -> return arr

listToJSArray :: ToJSRef a => [a] -> IO (JSArray a)
listToJSArray = toJSArray deconstr
        where deconstr (x : xs) = Just (x, xs)
              deconstr [] = Nothing

loadBuffer :: Ctx -> Word -> ArrayBufferView -> IO Buffer
loadBuffer ctx ty bufData =
        do buffer <- createBuffer ctx
           bindBuffer ctx ty buffer
           bufferData ctx ty bufData gl_STATIC_DRAW
           bindBuffer ctx ty noBuffer
           return buffer

loadShaders :: Ctx -> String -> String -> IO Program
loadShaders ctx vsSrc fsSrc =
        do program <- createProgram ctx
           vs <- loadShader ctx gl_VERTEX_SHADER vsSrc 
           fs <- loadShader ctx gl_FRAGMENT_SHADER fsSrc 
           attachShader ctx program vs
           attachShader ctx program fs
           bindAttribLocation ctx program posAttribLoc $ toJSString "pos"
           bindAttribLocation ctx program normalAttribLoc $ toJSString "normal"
           bindAttribLocation ctx program uvAttribLoc $ toJSString "uv"
           linkProgram ctx program
           useProgram ctx program
           return program

posAttribLoc :: Num a => a
posAttribLoc = 0

normalAttribLoc :: Num a => a
normalAttribLoc = 1

uvAttribLoc :: Num a => a
uvAttribLoc = 2

loadShader :: Ctx -> Word -> String -> IO Shader
loadShader ctx ty src =
        do shader <- createShader ctx ty
           shaderSource ctx shader $ toJSString src
           compileShader ctx shader
           return shader

defVS :: String
defVS = "       attribute vec3 pos;                                     \
        \       attribute vec3 normal;                                  \
        \       attribute vec2 uv;                                      \
        \       varying vec4 vpos;                                      \
        \       varying vec2 vuv;                                       \
        \       uniform mat4 modelMatrix;                               \
        \       void main() {                                           \
        \               vpos = vec4(pos, 1.0);                          \
        \               vuv = uv;                                       \
        \               gl_Position = modelMatrix * vpos;               \
        \       }                                                       "

defFS :: String
defFS = "       precision mediump float;                                \
        \       varying vec4 vpos;                                      \
        \       varying vec2 vuv;                                       \
        \       uniform sampler2D sampler;                              \
        \       void main() {                                           \
        \               gl_FragColor = texture2D(sampler,               \
        \                       vec2(vuv.s, 1.0 - vuv.t)                \
        \               );                                              \
        \       }                                                       "
