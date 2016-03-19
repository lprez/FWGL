{-# LANGUAGE MultiParamTypeClasses, TypeFamilies, NullaryTypeClasses #-}

module FWGL.Backend where

import Control.Concurrent (ThreadId)
import FRP.Yampa
import FWGL.Backend.GLES
import FWGL.Input

class BackendIO where
        -- | Where to draw (e.g. a window, a HTML5 canvas).
        type Canvas
        type BackendState

        -- TODO: loadImage may fail
        loadImage :: FilePath                           -- ^ Path or URL.
                  -> ((Image, Int, Int) -> IO ())       -- ^ Callback.
                  -> IO ()

        loadTextFile :: FilePath                        -- ^ Path or URL.
                     -> (Either String String -> IO ()) -- ^ Callback
                     -> IO ()

        initBackend :: IO BackendState

        createCanvas :: String -> BackendState -> IO (Canvas, Int, Int)

        -- | Set the size of the canvas/window.
        setCanvasSize :: Int -- ^ Width
                      -> Int -- ^ Height
                      -> Canvas
                      -> BackendState
                      -> IO ()

        -- | Set the title of the window.
        setCanvasTitle :: String -> Canvas -> BackendState -> IO ()

        setCanvasResizeCallback :: (Int -> Int -> IO ()) 
                                -> Canvas -> BackendState -> IO ()
        setCanvasRefreshCallback :: IO () -> Canvas -> BackendState -> IO ()

        -- | 'EventData's must be sorted in reverse chronological order.
        popInput :: a -> Canvas -> BackendState -> IO (Input a)
        getInput :: a -> Canvas -> BackendState -> IO (Input a)

        drawCanvas :: IO a              -- ^ Draw action.
                   -> Bool              -- ^ Swap buffers.
                   -> Canvas
                   -> BackendState
                   -> IO a

        refreshLoop :: Int  -- ^ FPS (not necessarily used).
                    -> Canvas
                    -> BackendState
                    -> IO ()

        -- | Time, in seconds.
        getTime :: BackendState -> IO Double

        terminateBackend :: BackendState -> IO ()
