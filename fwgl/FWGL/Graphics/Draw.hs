{-# LANGUAGE GADTs, DataKinds, FlexibleContexts, TypeSynonymInstances,
             FlexibleInstances, MultiParamTypeClasses #-}

module FWGL.Graphics.Draw (
        Draw,
        DrawState,
        execDraw,
        drawInit,
        drawBegin,
        drawLayer,
        drawObject,
        drawEnd,
        removeGeometry,
        removeTexture,
        removeProgram,
        textureUniform,
        textureSize,
        setProgram,
        resize,
        gl,
        subLayerToTexture,
        drawState
) where

import FWGL.Geometry
import FWGL.Graphics.Shapes
import FWGL.Graphics.Types
import FWGL.Graphics.Texture
import FWGL.Backend.IO
import FWGL.Internal.GL hiding (Texture, Program, UniformLocation)
import qualified FWGL.Internal.GL as GL
import FWGL.Internal.Resource
import FWGL.Shader.CPU
import FWGL.Shader.GLSL
import FWGL.Shader.Program hiding (program)
import FWGL.Vector

import Data.Bits ((.|.))
import Data.Hashable (Hashable)
import qualified Data.HashMap.Strict as H
import qualified Data.Vector as V
import Data.Typeable
import Data.Word (Word)
import Control.Applicative
import Control.Monad (when)
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.State

-- | Create a 'DrawState'.
drawInit :: (BackendIO, GLES)
         => Int         -- ^ Viewport width
         -> Int         -- ^ Viewport height
         -> GL DrawState
drawInit w h = do enable gl_DEPTH_TEST
                  enable gl_BLEND
                  blendFunc gl_SRC_ALPHA gl_ONE_MINUS_SRC_ALPHA
                  clearColor 0.0 0.0 0.0 1.0
                  depthFunc gl_LESS
                  resizeGL w h
                  return DrawState { program = Nothing
                                   , loadedProgram = Nothing
                                   , programs = newGLResMap
                                   , gpuMeshes = newGLResMap
                                   , uniforms = newGLResMap
                                   , textureImages = newGLResMap
                                   , activeTextures =
                                           V.replicate maxTexs Nothing
                                   , viewportSize = (w, h) }
        where newGLResMap :: (Hashable i, Resource i r GL) => ResMap i r
              newGLResMap = newResMap

              maxTexs = fromIntegral gl_MAX_COMBINED_TEXTURE_IMAGE_UNITS

-- | Execute a 'Draw' action.
execDraw :: Draw ()             -- ^ Action.
         -> DrawState           -- ^ State.
         -> GL DrawState
execDraw (Draw a) = execStateT a

-- | Get the 'DrawState'.
drawState :: Draw DrawState
drawState = Draw get

-- | Viewport.
resizeGL :: GLES
         => Int   -- ^ Width.
         -> Int   -- ^ Height.
         -> GL ()
resizeGL w h = viewport 0 0 (fromIntegral w) (fromIntegral h)

-- | Viewport.
resize :: GLES
       => Int   -- ^ Width.
       -> Int   -- ^ Height.
       -> Draw ()
resize w h = do gl $ resizeGL w h
                Draw . modify $ \s -> s { viewportSize = (w, h) }

-- | Clear the buffers.
drawBegin :: GLES => Draw ()
drawBegin = do freeActiveTextures
               gl . clear $ gl_COLOR_BUFFER_BIT .|. gl_DEPTH_BUFFER_BIT

drawEnd :: GLES => Draw ()
drawEnd = return ()

-- | Delete a 'Geometry' from the GPU.
removeGeometry :: GLES => Geometry is -> Draw Bool
removeGeometry = removeDrawResource gl gpuMeshes (\m s -> s { gpuMeshes = m })
                 . castGeometry

-- | Delete a 'Texture' from the GPU.
removeTexture :: BackendIO => Texture -> Draw Bool
removeTexture (TextureImage i) = removeDrawResource gl textureImages
                                        (\m s -> s { textureImages = m }) i
removeTexture (TextureLoaded l) = do gl $ unloadResource
                                          (Nothing :: Maybe TextureImage) l
                                     return True

-- | Delete a 'Program' from the GPU.
removeProgram :: GLES => Program gs is -> Draw Bool
removeProgram = removeDrawResource gl programs (\m s -> s { programs = m })
                . castProgram

-- | Draw a 'Layer'.
drawLayer :: (GLES, BackendIO) => Layer -> Draw ()
drawLayer (Layer prg obj) = setProgram prg >> drawObject obj
drawLayer (SubLayer stype w' h' sub sup) =
        do t <- subLayerToTexture stype w h sub
           mapM_ drawLayer $ sup t
           removeTexture t
           return ()
        where w = fromIntegral w'
              h = fromIntegral h'

drawObject :: (GLES, BackendIO) => Object gs is -> Draw ()
drawObject ObjectEmpty = return ()
drawObject (ObjectMesh g) = withRes_ (getGPUGeometry $ castGeometry g)
                                   drawGPUGeometry
drawObject (ObjectGlobal g c o) = c >>= uniform g >> drawObject o
drawObject (ObjectAppend o o') = drawObject o >> drawObject o'

uniform :: (GLES, Typeable g, UniformCPU c g) => g -> c -> Draw ()
uniform g c = withRes_ (getUniform g)
                       $ \(UniformLocation l) -> gl $ setUniform l g c

textureUniform :: (GLES, BackendIO) => Texture -> Draw ActiveTexture
textureUniform tex = withRes (getTexture tex) (return $ ActiveTexture 0)
                                 $ \(LoadedTexture _ _ wtex) ->
                                        do at <- makeActive tex
                                           gl $ bindTexture gl_TEXTURE_2D wtex
                                           return at

-- | Get the dimensions of a 'Texture'.
textureSize :: (GLES, BackendIO, Num a) => Texture -> Draw (a, a)
textureSize tex = withRes (getTexture tex) (return (0, 0))
                          $ \(LoadedTexture w h _) -> return ( fromIntegral w
                                                             , fromIntegral h)

-- | Set the program.
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
getTexture (TextureLoaded l) = return $ Loaded l
getTexture (TextureImage t) = getTextureImage t

getTextureImage :: (GLES, BackendIO) => TextureImage
                -> Draw (ResStatus LoadedTexture)
getTextureImage = getDrawResource gl textureImages
                                     (\ m s -> s { textureImages = m })

getProgram :: GLES => Program '[] '[] -> Draw (ResStatus LoadedProgram)
getProgram = getDrawResource gl programs (\ m s -> s { programs = m })

freeActiveTextures :: Draw ()
freeActiveTextures = Draw . modify $ \ds ->
        ds { activeTextures = V.map (const Nothing) $ activeTextures ds }

makeActive :: GLES => Texture -> Draw ActiveTexture
makeActive t = do ats <- activeTextures <$> Draw get
                  let at@(ActiveTexture atn) =
                        case V.elemIndex (Just t) ats of
                                Just n -> ActiveTexture $ fi n
                                Nothing ->
                                        case V.elemIndex Nothing ats of
                                             Just n -> ActiveTexture $ fi n
                                             -- TODO: Draw () error reporting
                                             Nothing -> ActiveTexture 0
                  gl . activeTexture $ gl_TEXTURE0 + fi atn
                  Draw . modify $ \ds ->
                          ds { activeTextures = ats V.// [(fi atn, Just t)] }
                  return at
        where fi :: (Integral a, Integral b) => a -> b
              fi = fromIntegral

subLayerToTexture :: (GLES, BackendIO, Integral a)
                  => SubLayerType
                  -> a -- | Width
                  -> a -- | Height
                  -> Layer
                  -> Draw Texture
subLayerToTexture stype wp hp layer =
        TextureLoaded . LoadedTexture w h <$>
        renderToTexture internalFormat format ptype attachment w h layer
        where (w, h) = (fromIntegral wp, fromIntegral hp)
              (internalFormat, format, ptype, attachment) =
                        case stype of
                              ColorSubLayer -> ( fromIntegral gl_RGBA
                                               , gl_RGBA
                                               , gl_UNSIGNED_BYTE
                                               , gl_COLOR_ATTACHMENT0 )
                              DepthSubLayer -> ( fromIntegral gl_DEPTH_COMPONENT
                                               , gl_DEPTH_COMPONENT
                                               , gl_UNSIGNED_SHORT
                                               , gl_DEPTH_ATTACHMENT )

renderToTexture :: (GLES, BackendIO) => GLInt -> GLEnum -> GLEnum
                -> GLEnum -> GLSize -> GLSize -> Layer -> Draw GL.Texture
renderToTexture internalFormat format pixelType attachment w h layer = do
        fb <- gl createFramebuffer
        t <- gl emptyTexture
        (sw, sh) <- viewportSize <$> Draw get

        gl $ do arr <- liftIO $ noArray
                bindTexture gl_TEXTURE_2D t
                texImage2DBuffer gl_TEXTURE_2D 0 internalFormat w 
                                 h 0 format pixelType arr

                bindFramebuffer gl_FRAMEBUFFER fb
                framebufferTexture2D gl_FRAMEBUFFER attachment
                                     gl_TEXTURE_2D t 0

        gl $ resizeGL (fromIntegral w) (fromIntegral h)
        drawBegin
        drawLayer layer
        drawEnd
        gl $ resizeGL sw sh

        gl $ deleteFramebuffer fb

        return t

getDrawResource :: (Resource i r m, Hashable i)
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

removeDrawResource :: (Resource i r m, Hashable i)
                   => (m (Bool, ResMap i r) -> Draw (Bool, ResMap i r))
                   -> (DrawState -> ResMap i r)
                   -> (ResMap i r -> DrawState -> DrawState)
                   -> i
                   -> Draw Bool
removeDrawResource lft mg ms i = do
        s <- Draw get
        (removed, map) <- lft . removeResource i $ mg s
        Draw . put $ ms map s
        return removed

drawGPUGeometry :: GLES => GPUGeometry -> Draw ()
drawGPUGeometry (GPUGeometry abs eb ec) =
        loadedProgram <$> Draw get >>= \mlp -> case mlp of
                Nothing -> return ()
                Just (LoadedProgram _ locs _) -> gl $ do
                        bindBuffer gl_ARRAY_BUFFER noBuffer
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

-- | Perform a 'GL' action in the 'Draw' monad.
gl :: GL a -> Draw a
gl = Draw . lift
