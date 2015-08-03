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

        createCanvas :: IO Canvas

        setCanvasSize :: Int -- ^ Width
                      -> Int -- ^ Height
                      -> Canvas -> IO ()

        setCanvasTitle :: String -> IO ()

        setCanvasResizeCallback :: (Int -> Int -> IO ()) -> IO ()
        setCanvasRefreshCallback :: IO () -> IO ()

        getCanvasContext :: Canvas -> IO Ctx

        popInput :: Canvas -> IO (Input ())
        getInput :: Canvas -> IO (Input ())

        drawCanvas :: (Ctx -> IO ()) -> Canvas -> IO ()
        updateCanvas :: Canvas -> IO ()

        -- | In seconds.
        getTime :: IO Double
