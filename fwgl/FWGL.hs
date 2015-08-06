{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}

{-|
    The main module. You should also import a backend:

        * FWGL.Backend.JavaScript: GHCJS/WebGL backend (contained in fwgl-javascript)
        * FWGL.Backend.GLFW.GL20: GLFW/OpenGL 2.0 backend (contained in fwgl-glfw)


    And a graphics system:

        * "FWGL.Graphics.D2": 2D graphics
        * "FWGL.Graphics.D3": 3D graphics
        * "FWGL.Graphics.Custom": advanced custom graphics


    "FWGL.Shader" contains the EDSL to make custom shaders.

    Import "FWGL.Internal.GL" if you want to use raw GL actions.
-}
module FWGL (
        module FWGL.Audio,
        module FWGL.Input,
        module FWGL.Utils,
        module FRP.Yampa,
        -- * FRP interface
        run,
        run',
        -- ** Effects
        Output,
        (.>),
        draw,
        io,
        fastStep,
        freeGeometry,
        freeTexture,
        freeProgram,
        drawM,
        -- TODO: resize, title ...
        -- * OBJ loading
        -- loadTextFile
        loadOBJ,
        loadOBJAsync,
        -- * Draw monad
        Draw,
        SubLayerType(..),
        drawLayer,
        subLayerToTexture,
        removeGeometry,
        removeTexture,
        removeProgram,
        gl,
        liftIO,
        drawState,
        execDraw,
        textureUniform,
        textureSize,
        -- textureData
        -- * IO Interface
) where

import Data.IORef
import qualified Data.HashMap.Strict as H
import Control.Concurrent
import Control.Monad
import Control.Monad.IO.Class
import FWGL.Audio
import FWGL.Backend hiding (Texture, Program)
import FWGL.Input
import FWGL.Internal.GL (evalGL)
import FWGL.Geometry (Geometry3)
import FWGL.Geometry.OBJ
import FWGL.Graphics.Draw as D
import FWGL.Graphics.Types
import FWGL.Shader.Program (Program)
import FWGL.Utils
import FRP.Yampa

-- | The general output.
data Output = Output Bool (Draw ())

-- | Sequence two 'Output' effects.
(.>) :: Output -> Output -> Output
Output r d .> Output r' d' = Output (r || r') (d >> d')

-- | Draw some layers.
draw :: BackendIO => [Layer] -> Output
draw = Output False . mapM_ drawLayer

-- | Use this when you want the next sample to be performed immediately
-- (e.g. when you need to produce some computationally expensive effectful input
-- at the request of the signal function). You can place it anywhere in the
-- action sequence. You shouldn't draw anything if you use it.
fastStep :: Output
fastStep = Output True (return ())

-- | Perform an IO action.
io :: IO () -> Output
io = Output False . liftIO

-- | Perform an action in the Draw monad.
drawM :: Draw () -> Output
drawM = Output False

-- | Delete a 'Geometry' from the GPU.
freeGeometry :: BackendIO => Geometry i -> Output
freeGeometry = Output False . void . removeGeometry

-- | Delete a 'Texture' from the GPU.
freeTexture :: BackendIO => Texture -> Output
freeTexture = Output False . void . removeTexture

-- | Delete a 'Program' from the GPU.
freeProgram :: BackendIO => Program g i -> Output
freeProgram = Output False . void . removeProgram

-- | Run a FWGL program.
run :: BackendIO
    => SF (Input ()) Output  -- ^ Main signal
    -> IO ()
run = run' $ return ()

-- | Run a FWGL program, using custom inputs.
run' :: BackendIO
     => IO inp                -- ^ An IO effect generating the custom inputs.
     -> SF (Input inp) Output
     -> IO ()
run' customInput sigf =
        do initBackend
           (canvas, w, h) <- createCanvas

           initCustom <- customInput
           lastTimeRef <- getTime >>= newIORef
           drawStateVar <- drawCanvas (initState w h) False canvas >>= newMVar
           reactStateRef <- reactInit (return $ initInput w h initCustom)
                                      (\reactStateRef _ -> actuate lastTimeRef
                                                                   reactStateRef
                                                                   canvas
                                                                   drawStateVar)
                                      sigf

           setCanvasResizeCallback (resizeCb drawStateVar canvas) canvas

           setCanvasRefreshCallback (refreshCb lastTimeRef reactStateRef canvas)
                                    canvas

           refreshLoop 60 canvas

        where initState w h = evalGL $ drawInit w h

              resizeCb drawStateVar canvas w h =
                      do drawState <- takeMVar drawStateVar
                         drawState' <- drawCanvas
                                        (\ctx -> flip evalGL ctx $
                                                execDraw (D.resize w h)
                                                         drawState
                                        ) False canvas
                         putMVar drawStateVar drawState'

              refreshCb lastTimeRef reactStateRef canvas =
                      do tm <- readIORef lastTimeRef
                         tm' <- getTime
                         custom <- customInput
                         inp <- popInput custom canvas
                         react reactStateRef ((tm' - tm) * 1000, Just inp)
                         writeIORef lastTimeRef tm'

              actuate lastTimeRef reactStateRef canvas drawStateVar
                      (Output re drawAct) =
                      do drawCanvas drawTo True canvas
                         when re $ refreshCb lastTimeRef reactStateRef canvas
                         return False
                      where drawTo ctx =
                              modifyMVar_ drawStateVar $ \s ->
                                        flip evalGL ctx . flip execDraw s $
                                                do unless re drawBegin
                                                   drawAct
                                                   unless re drawEnd
        
              initInput w h = Input $ H.singleton Resize [
                        emptyEventData {
                                dataFramebufferSize = Just (w, h)
                        }]

              emptyEventData = EventData {
                                dataFramebufferSize = Nothing,
                                dataPointer = Nothing,
                                dataButton = Nothing,
                                dataKey = Nothing }

-- | Load a model from an OBJ file asynchronously.
loadOBJAsync :: BackendIO 
             => FilePath
             -> (Either String (Geometry Geometry3) -> IO ())
             -> IO ()
loadOBJAsync fp k = loadTextFile fp $
                       \e -> case e of
                                  Left err -> k $ Left err
                                  Right str -> k . Right . geometryOBJ
                                                 . parseOBJ $ str

-- | Load a model from an OBJ file.
loadOBJ :: BackendIO => FilePath -> IO (Either String (Geometry Geometry3))
loadOBJ fp = do var <- newEmptyMVar
                loadOBJAsync fp $ putMVar var
                takeMVar var
