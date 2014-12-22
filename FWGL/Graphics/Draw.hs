{-# LANGUAGE GADTs, DataKinds, GeneralizedNewtypeDeriving, FlexibleContexts,
             TypeSynonymInstances, FlexibleInstances, MultiParamTypeClasses #-}

module FWGL.Graphics.Draw (
        Draw,
        DrawState,
        execDraw,
        drawInit,
        drawBegin,
        drawScene,
        setProgram,
        resize
) where

import FWGL.Geometry
import FWGL.Graphics.Shapes
import FWGL.Graphics.Types
import FWGL.Backend.IO
import FWGL.Internal.GL hiding (Texture, Program, UniformLocation)
import qualified FWGL.Internal.GL as GL
import FWGL.Internal.Resource
import FWGL.Shader.CPU
import FWGL.Shader.GLSL
import FWGL.Shader.Program hiding (program)
import FWGL.Texture
import FWGL.Vector

import qualified Data.HashMap.Strict as H
import Data.Typeable
import Data.Word (Word)
import Control.Applicative
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.State

newtype UniformLocation = UniformLocation GL.UniformLocation

data DrawState = DrawState {
        program :: Program '[] '[],
        loadedProgram :: LoadedProgram,
        programs :: ResMap (Program '[] '[]) LoadedProgram,
        uniforms :: ResMap (LoadedProgram, String) UniformLocation,
        gpuMeshes :: ResMap (Geometry '[]) GPUGeometry,
        textures :: ResMap Texture LoadedTexture
}

newtype Draw a = Draw { unDraw :: StateT DrawState GL a }
        deriving (Functor, Applicative, Monad, MonadIO)

-- TODO: no width/height
drawInit :: (BackendIO, GLES) => Int -> Int -> GL DrawState
drawInit w h = do enable gl_DEPTH_TEST
                  depthFunc gl_LESS
                  clearColor 0.0 0.0 0.0 1.0
                  resize w h
                  return DrawState { program = undefined
                                   , loadedProgram = undefined
                                   , programs = newGLResMap
                                   , gpuMeshes = newGLResMap
                                   , uniforms = newGLResMap
                                   , textures = newGLResMap }
        where newGLResMap :: Resource i r GL => ResMap i r
              newGLResMap = newResMap

execDraw :: Draw () -> DrawState -> GL DrawState
execDraw (Draw a) = execStateT a

resize :: GLES => Int -> Int -> GL ()
resize = viewport 0 0

drawBegin :: GLES => Draw ()
drawBegin = gl $ clear gl_COLOR_BUFFER_BIT

drawScene :: (GLES, BackendIO) => Scene -> Draw ()
drawScene (Scene prg obj) = setProgram prg >> drawObject obj

drawObject :: (GLES, BackendIO) => Object gs is -> Draw ()
drawObject ObjectEmpty = return ()
drawObject (ObjectMesh m) = drawMesh m
drawObject (ObjectTexture t o) = setTexture t >> drawObject o
drawObject (ObjectGlobal g c o) = uniform g c >> drawObject o
drawObject (ObjectAppend o o') = drawObject o >> drawObject o'

{-
           wtex <- getTexture tex
           gl $ do matarr <- encodeM4 trans
                   uniformMatrix4fv modelUni False matarr
                   activeTexture gl_TEXTURE0
                   bindTexture gl_TEXTURE_2D wtex
                   uniform1i texUni 0
-}

drawMesh :: GLES => Mesh is -> Draw ()
drawMesh Empty = return ()
drawMesh Cube = drawMesh (StaticGeom cubeGeometry)
drawMesh (StaticGeom g) = getGPUGeometry (castGeometry g) >>= drawGPUGeometry
drawMesh (DynamicGeom _ _) = error "drawMesh DynamicGeom: unsupported"
-- drawMesh (DynamicGeom d g) = delete {- removeResource -} d >> drawMesh (StaticGeom g)

uniform :: (GLES, Typeable g, UniformCPU c g) => g -> c -> Draw ()
uniform g c = getUniform g >>= \(UniformLocation l) -> gl $ setUniform l g c

setTexture :: (GLES, BackendIO) => Texture -> Draw ()
setTexture tex = do (LoadedTexture wtex) <- getTexture tex
                    gl $ do activeTexture gl_TEXTURE0
                            bindTexture gl_TEXTURE_2D wtex
                    uniform (undefined :: Texture2) $ ActiveTexture 0

setProgram :: GLES => Program g i -> Draw ()
-- TODO: controllare se il programma è uguale (sul segnale)
setProgram p = do lp <- getProgram $ castProgram p
                  Draw . modify $ \s -> s {
                        program = castProgram p,
                        loadedProgram = lp
                  }
                  

getUniform :: (Typeable a, GLES) => a -> Draw UniformLocation
getUniform g = do prg <- loadedProgram <$> Draw get
                  getDrawResource gl uniforms (\ m s -> s { uniforms = m })
                                  (prg, globalName g)

getGPUGeometry :: GLES => Geometry '[] -> Draw GPUGeometry
getGPUGeometry = getDrawResource gl gpuMeshes (\ m s -> s { gpuMeshes = m })

getTexture :: (GLES, BackendIO) => Texture -> Draw LoadedTexture
getTexture = getDrawResource gl textures (\ m s -> s { textures = m })

getProgram :: GLES => Program '[] '[] -> Draw LoadedProgram
getProgram = getDrawResource gl programs (\ m s -> s { programs = m })

getDrawResource :: Resource i r m
                => (m (r, ResMap i r) -> Draw (r, ResMap i r))
                -> (DrawState -> ResMap i r)
                -> (ResMap i r -> DrawState -> DrawState)
                -> i
                -> Draw r
getDrawResource lft mg ms i = do
        s <- Draw get
        (r, map) <- lft . getResource i $ mg s
        Draw . put $ ms map s
        return r

drawGPUGeometry :: GLES => GPUGeometry -> Draw ()
drawGPUGeometry (GPUGeometry abs eb ec) =
        loadedProgram <$> Draw get >>= \(LoadedProgram _ locs _) -> gl $ do
           enabledLocs <- mapM (\(nm, buf, setAttr) ->
                                let loc = locs H.! nm in
                                     do bindBuffer gl_ARRAY_BUFFER buf
                                        enableVertexAttribArray $
                                                fromIntegral loc
                                        setAttr $ fromIntegral loc
                                        return loc
                               ) abs

           bindBuffer gl_ELEMENT_ARRAY_BUFFER eb
           drawElements gl_TRIANGLES ec gl_UNSIGNED_SHORT 0
           bindBuffer gl_ELEMENT_ARRAY_BUFFER noBuffer

           mapM_ (disableVertexAttribArray . fromIntegral) enabledLocs
           bindBuffer gl_ARRAY_BUFFER noBuffer

{-
-}

instance GLES => Resource (LoadedProgram, String) UniformLocation GL where
        loadResource (LoadedProgram prg _ _, g) =
                fmap UniformLocation . getUniformLocation prg $ toGLString g
        unloadResource _ _ = return ()

gl :: GL a -> Draw a
gl = Draw . lift
