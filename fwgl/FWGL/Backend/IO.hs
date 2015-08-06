{-# LANGUAGE MultiParamTypeClasses, TypeFamilies, NullaryTypeClasses #-}

module FWGL.Backend.IO where

import FRP.Yampa
import FWGL.Backend.GLES
import FWGL.Input

class GLES => BackendIO where
        -- | Where to draw (e.g. a window, a HTML5 canvas).
        type Canvas

        -- TODO: loadImage may fail
        loadImage :: FilePath                           -- ^ Path or URL.
                  -> ((Image, Int, Int) -> IO ())       -- ^ Callback.
                  -> IO ()

        loadTextFile :: FilePath                        -- ^ Path or URL.
                     -> (Either String String -> IO ()) -- ^ Callback
                     -> IO ()

        initBackend :: IO ()

        createCanvas :: IO (Canvas, Int, Int)

        -- | Set the size of the canvas/window.
        setCanvasSize :: Int -- ^ Width
                      -> Int -- ^ Height
                      -> Canvas
                      -> IO ()

        -- | Set the title of the window.
        setCanvasTitle :: String -> Canvas -> IO ()

        setCanvasResizeCallback :: (Int -> Int -> IO ()) -> Canvas -> IO ()
        setCanvasRefreshCallback :: IO () -> Canvas -> IO ()

        popInput :: a -> Canvas -> IO (Input a)
        getInput :: a -> Canvas -> IO (Input a)

        drawCanvas :: (Ctx -> IO a)     -- ^ Draw action.
                   -> Bool              -- ^ Swap buffers.
                   -> Canvas
                   -> IO a

        -- | Not needed, if you use 'refreshLoop'.
        updateCanvas :: Canvas -> IO Bool

        refreshLoop :: Int  -- ^ FPS (not necessarily used).
                    -> Canvas
                    -> IO ()

        -- | Time, in milliseconds.
        getTime :: IO Double

        terminateBackend :: IO ()
