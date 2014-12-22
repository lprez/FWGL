{-# LANGUAGE NullaryTypeClasses, TypeFamilies #-}

module FWGL.Backend.IO where

import FRP.Yampa
import FWGL.Backend.GLES
import FWGL.Input

class GLES => BackendIO where
        loadImage :: String -> (Image -> IO ()) -> IO ()

        setup :: (Int -> Int -> Ctx -> IO state)
              -> (out -> Ctx -> state -> IO state)
              -> SF Input out
              -> IO ()
