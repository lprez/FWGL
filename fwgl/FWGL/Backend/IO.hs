{-# LANGUAGE MultiParamTypeClasses, TypeFamilies, NullaryTypeClasses #-}

module FWGL.Backend.IO where

import FRP.Yampa
import FWGL.Backend.GLES
import FWGL.Input

class GLES => BackendIO where
        -- TODO: loadImage may fail
        loadImage :: FilePath -> ((Image, Int, Int) -> IO ()) -> IO ()

        loadTextFile :: FilePath -> (Either String String -> IO ()) -> IO ()

        setup :: (Int -> Int -> Ctx -> IO state)
              -> (out -> Ctx -> state -> IO state)
              -> IO inp
              -> SF (Input inp) out
              -> IO ()
