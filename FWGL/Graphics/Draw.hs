{-# LANGUAGE GADTs, DataKinds, FlexibleContexts, TypeSynonymInstances,
             FlexibleInstances, MultiParamTypeClasses #-}

module FWGL.Graphics.Draw (
        Draw,
        DrawState,
        execDraw,
        drawInit,
        drawBegin,
        drawLayer,
        textureUniform,
        textureSize,
        setProgram,
        resize
) where

import FWGL.Geometry
import FWGL.Graphics.Shapes
import FWGL.Graphics.Types
import FWGL.Backend.IO
import FWGL.Internal.GL hiding (Texture, Program, UniformLocation)
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
import Control.Monad (when)
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.State

drawInit :: (BackendIO, GLES) => Int -> Int -> GL DrawState
drawInit w h = do enable gl_DEPTH_TEST
                  clearColor 0.0 0.0 0.0 1.0
                  depthFunc gl_LESS
                  resize w h
                  return DrawState { program = Nothing
                                   , loadedProgram = Nothing
                                   , programs = newGLResMap
                                   , gpuMeshes = newGLResMap
                                   , uniforms = newGLResMap
                                   , textures = newGLResMap }
        where newGLResMap :: Resource i r GL => ResMap i r
              newGLResMap = newResMap

execDraw :: Draw () -> DrawState -> GL DrawState
execDraw (Draw a) = execStateT a

resize :: GLES => Int -> Int -> GL ()
resize w h = viewport 0 0 (fromIntegral w) (fromIntegral h)

drawBegin :: GLES => Draw ()
drawBegin = gl $ clear gl_COLOR_BUFFER_BIT

drawLayer :: (GLES, BackendIO) => Layer -> Draw ()
drawLayer (Layer prg obj) = setProgram prg >> drawObject obj

drawObject :: (GLES, BackendIO) => Object gs is -> Draw ()
drawObject ObjectEmpty = return ()
drawObject (ObjectMesh m) = drawMesh m
drawObject (ObjectGlobal g c o) = c >>= uniform g >> drawObject o
drawObject (ObjectAppend o o') = drawObject o >> drawObject o'

drawMesh :: GLES => Mesh is -> Draw ()
drawMesh Empty = return ()
drawMesh Cube = drawMesh (StaticGeom cubeGeometry)
drawMesh (StaticGeom g) = withRes_ (getGPUGeometry $ castGeometry g)
                                   drawGPUGeometry
drawMesh (DynamicGeom _ _) = error "drawMesh DynamicGeom: unsupported"
-- drawMesh (DynamicGeom d g) = delete {- removeResource -} d >> drawMesh (StaticGeom g)

uniform :: (GLES, Typeable g, UniformCPU c g) => g -> c -> Draw ()
uniform g c = withRes_ (getUniform g)
                       $ \(UniformLocation l) -> gl $ setUniform l g c

textureUniform :: (GLES, BackendIO) => Texture -> Draw ActiveTexture
textureUniform tex = do withRes_ (getTexture tex)
                                 $ \(LoadedTexture _ _ wtex) ->
                                        gl $ do activeTexture gl_TEXTURE0
                                                bindTexture gl_TEXTURE_2D wtex
                        return $ ActiveTexture 0

textureSize :: (GLES, BackendIO, Num a) => Texture -> Draw (a, a)
textureSize tex = withRes (getTexture tex) (return (0, 0))
                          $ \(LoadedTexture w h _) -> return ( fromIntegral w
                                                             , fromIntegral h)

setProgram :: GLES => Program g i -> Draw ()
setProgram p = do current <- program <$> Draw get
                  when (current /= Just (castProgram p)) $
                        withRes_ (getProgram $ castProgram p) $
                                \lp@(LoadedProgram glp _ _) -> do
                                   Draw . modify $ \s -> s {
                                           program = Just $ castProgram p,
                                           loadedProgram = Just lp
                                   }
                                   gl $ useProgram glp

withRes_ :: Draw (ResStatus a) -> (a -> Draw ()) -> Draw ()
withRes_ drs = withRes drs $ return ()

withRes :: Draw (ResStatus a) -> Draw b -> (a -> Draw b) -> Draw b
withRes drs u l = drs >>= \rs -> case rs of
                                        Loaded r -> l r
                                        _ -> u

getUniform :: (Typeable a, GLES) => a -> Draw (ResStatus UniformLocation)
getUniform g = do mprg <- loadedProgram <$> Draw get
                  case mprg of
                          Just prg ->
                                  getDrawResource gl uniforms
                                                  (\ m s -> s { uniforms = m })
                                                  (prg, globalName g)
                          Nothing -> return $ Error "No loaded program."

getGPUGeometry :: GLES => Geometry '[] -> Draw (ResStatus GPUGeometry)
getGPUGeometry = getDrawResource gl gpuMeshes (\ m s -> s { gpuMeshes = m })

getTexture :: (GLES, BackendIO) => Texture -> Draw (ResStatus LoadedTexture)
getTexture = getDrawResource gl textures (\ m s -> s { textures = m })

getProgram :: GLES => Program '[] '[] -> Draw (ResStatus LoadedProgram)
getProgram = getDrawResource gl programs (\ m s -> s { programs = m })

getDrawResource :: Resource i r m
                => (m (ResStatus r, ResMap i r)
                    -> Draw (ResStatus r, ResMap i r))
                -> (DrawState -> ResMap i r)
                -> (ResMap i r -> DrawState -> DrawState)
                -> i
                -> Draw (ResStatus r)
getDrawResource lft mg ms i = do
        s <- Draw get
        (r, map) <- lft . getResource i $ mg s
        Draw . put $ ms map s
        return r

drawGPUGeometry :: GLES => GPUGeometry -> Draw ()
drawGPUGeometry (GPUGeometry abs eb ec) =
        loadedProgram <$> Draw get >>= \mlp -> case mlp of
                Nothing -> return ()
                Just (LoadedProgram _ locs _) -> gl $ do
                        enabledLocs <- mapM (\(nm, buf, setAttr) ->
                                             let loc = locs H.! nm in
                                                  do bindBuffer gl_ARRAY_BUFFER
                                                                buf
                                                     enableVertexAttribArray $
                                                             fromIntegral loc
                                                     setAttr $ fromIntegral loc
                                                     return loc
                                            ) abs

                        bindBuffer gl_ELEMENT_ARRAY_BUFFER eb
                        drawElements gl_TRIANGLES (fromIntegral ec)
                                     gl_UNSIGNED_SHORT nullGLPtr
                        bindBuffer gl_ELEMENT_ARRAY_BUFFER noBuffer

                        mapM_ (disableVertexAttribArray . fromIntegral)
                              enabledLocs
                        bindBuffer gl_ARRAY_BUFFER noBuffer

instance GLES => Resource (LoadedProgram, String) UniformLocation GL where
        loadResource (LoadedProgram prg _ _, g) f =
                do loc <- getUniformLocation prg $ toGLString g
                   f . Right $ UniformLocation loc
        unloadResource _ _ = return ()

gl :: GL a -> Draw a
gl = Draw . lift
