{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies,
             GeneralizedNewtypeDeriving #-}

{-|
    The main module. You should also import a backend:

        * FWGL.Backend.JavaScript: GHCJS/WebGL backend (contained in fwgl-javascript)
        * FWGL.Backend.GLFW.GL20: GLFW/OpenGL 2.0 backend (contained in fwgl-glfw)


    And a graphics system:

        * "FWGL.Graphics.D2": 2D graphics
        * "FWGL.Graphics.D3": 3D graphics
        * "FWGL.Graphics.Custom": advanced custom graphics


    "FWGL.Shader" contains the EDSL to make custom shaders.

    Import "FWGL.Internal.GL" if you want to use the raw GL commands.
-}
module FWGL (
        module FWGL.Audio,
        module FWGL.Input,
        module FWGL.Utils,
        module FRP.Yampa,
        FWGL(..),
        fwgl,
        mapIO,
        -- * FRP interface
        run,
        run',
        runTo,
        -- ** Effects
        Output(..),
        (.>),
        draw,
        io,
        setSize,
        setTitle,
        fastStep,
        freeGeometry,
        freeTexture,
        freeProgram,
        drawM,
        -- * File loading
        loadOBJ,
        loadOBJAsync,
        loadTextFileAsync,
        -- * Draw monad
        Draw,
        drawLayer,
        drawObject,
        setProgram,
        -- ** Memory functions
        removeGeometry,
        removeTexture,
        removeProgram,
        -- ** Texture functions
        textureUniform,
        textureSize,
        -- textureData
        -- ** Lifting functions
        gl,
        liftIO,
        -- ** Other functions
        resizeViewport,
        renderLayer,
        outDraw,
        {-
        -- * Effectful Interface
        runDraw,
        setCanvasSize,
        setCanvasTitle,
        getTime
        -}
) where

import Data.IORef
import qualified Data.HashMap.Strict as H
import Control.Concurrent
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Reader
import Data.Vect.Float
import FWGL.Audio
import FWGL.Backend hiding (Texture, Program)
import FWGL.Input
import FWGL.Internal.GL (evalGL)
import FWGL.Geometry (Geometry3)
import FWGL.Geometry.OBJ
import FWGL.Graphics.Draw
import FWGL.Graphics.Types
import FWGL.Shader.Program (Program)
import FWGL.Utils
import FRP.Yampa

-- | The general output.
data Output = Output Bool (Canvas -> BackendState -> Draw ())

-- | Sequence two 'Output' effects.
(.>) :: Output -> Output -> Output
Output r d .> Output r' d' = Output (r || r') (\c b -> d c b >> d' c b)

-- | Draw some layers.
draw :: BackendIO => [Layer] -> Output
draw ls = Output False $ \_ _ -> mapM_ drawLayer ls

-- | Use this when you want the next sample to be performed immediately
-- (e.g. when you need to produce some computationally expensive effectful input
-- at the request of the signal function). You can place it anywhere in the
-- action sequence. You shouldn't draw anything if you use it.
fastStep :: Output
fastStep = Output True $ \_ _ -> return ()

-- | Perform an IO action.
io :: IO () -> Output
io c = Output False $ \_ _ -> liftIO c

-- | Perform a 'Draw' action.
drawM :: Draw () -> Output
drawM c = Output False $ \_ _ -> c

-- | Perform an 'Output' action in the Draw monad.
outDraw :: Output -> (Draw () -> Draw ()) -> Output
outDraw (Output r act) k = Output r $ \canvas -> k . act canvas

-- | Delete a 'Geometry' from the GPU.
freeGeometry :: BackendIO => Geometry i -> Output
freeGeometry g = Output False $ \_ _ -> void $ removeGeometry g

-- | Delete a 'Texture' from the GPU.
freeTexture :: BackendIO => Texture -> Output
freeTexture t = Output False $ \_ _ -> void $ removeTexture t

-- | Delete a 'Program' from the GPU.
freeProgram :: BackendIO => Program g i -> Output
freeProgram p = Output False $ \_ _ -> void $ removeProgram p

-- | Set canvas/window size.
setSize :: BackendIO
        => Int -- ^ Width
        -> Int -- ^ Height
        -> Output
setSize w h = Output False $ \canvas bs -> liftIO $ setCanvasSize w h canvas bs

-- | Set window title.
setTitle :: BackendIO => String -> Output
setTitle title = Output False $
        \canvas bs -> liftIO $ setCanvasTitle title canvas bs

newtype FWGL a = FWGL (ReaderT BackendState IO a)
        deriving (Functor, Applicative, Monad, MonadIO)

-- | Initialize the FWGL backend, run the action and terminate it.
fwgl :: BackendIO => FWGL () -> IO ()
fwgl (FWGL a) = initBackend >>= \bs -> runReaderT a bs >> terminateBackend bs

-- | Useful for functions like 'forkIO' and 'forkOS'. Note that, while drawing
-- on an unbound thread blocks drawing on any other thread, bound threads block
-- only unbound threads.
mapIO :: (IO a -> IO b) -> FWGL a -> FWGL b
mapIO f (FWGL a) = FWGL ask >>= liftIO . f . runReaderT a

-- | Run a FWGL program on a new canvas/window.
run :: BackendIO
    => SF (Input ()) Output  -- ^ Main signal
    -> FWGL ()
run = run' $ return ()

-- | Run a FWGL program, using custom inputs.
run' :: BackendIO
     => IO inp                -- ^ An IO effect generating the custom inputs.
     -> SF (Input inp) Output
     -> FWGL ()
run' = runTo "canvas"

-- | Run a FWGL program, using custom inputs and a specified canvas.
runTo :: BackendIO
      => String -- ^ Destination canvas (eg. "#myCanvasId"). This only has
                -- meaning on the JavaScript backend.
      -> IO inp -- ^ An IO effect generating the custom inputs.
      -> SF (Input inp) Output
      -> FWGL ()
runTo dest customInput sigf = FWGL $ ask >>= \bs -> liftIO $
        do (canvas, w, h) <- createCanvas dest bs

           initCustom <- customInput
           lastTimeRef <- getTime bs >>= newIORef
           drawStateVar <- drawCanvas (initState w h canvas) False canvas bs
                           >>= newMVar
           reactStateRef <- reactInit (return $ initInput w h initCustom)
                                      (\reactStateRef _ -> actuate lastTimeRef
                                                                   reactStateRef
                                                                   canvas
                                                                   bs
                                                                   drawStateVar)
                                      sigf

           setCanvasResizeCallback (resizeCb drawStateVar canvas bs) canvas bs

           setCanvasRefreshCallback (refreshCb lastTimeRef reactStateRef
                                               canvas bs)
                                    canvas bs

           refreshLoop 60 canvas bs

        where initState w h canvas = evalGL $ drawInit w h canvas

              resizeCb drawStateVar canvas bs w h =
                      drawCanvas (\ctx -> modifyMVar_ drawStateVar
                        $ \drawState -> flip evalGL ctx $
                                                execDraw (resizeViewport w h)
                                                         drawState
                                            
                                  ) False canvas bs

              refreshCb lastTimeRef reactStateRef canvas bs =
                      do tm <- readIORef lastTimeRef
                         tm' <- getTime bs
                         custom <- customInput
                         inp <- popInput custom canvas bs
                         react reactStateRef ((tm' - tm) * 1000, Just inp)
                         writeIORef lastTimeRef tm'

              actuate lastTimeRef reactStateRef canvas bs drawStateVar
                      (Output re drawAct) =
                      do drawCanvas drawTo True canvas bs
                         when re $ refreshCb lastTimeRef reactStateRef canvas bs
                         return False
                      where drawTo ctx =
                              modifyMVar_ drawStateVar $ \s ->
                                     flip evalGL ctx . flip execDraw s $
                                                do unless re drawBegin
                                                   drawAct canvas bs
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
             => FilePath -- ^ Path or URL.
             -> (Either String (Geometry Geometry3) -> IO ()) -- ^ Callback.
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

-- | Load a file asynchronously.
loadTextFileAsync :: BackendIO
                  => FilePath                           -- ^ Path or URL.
                  -> (Either String String -> IO ())    -- ^ Callback.
                  -> IO ()
loadTextFileAsync = loadTextFile
