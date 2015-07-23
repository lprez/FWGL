{-# LANGUAGE NullaryTypeClasses, TypeFamilies #-}

module FWGL.Backend.GLFW.GL20 () where

import FWGL.Backend.IO
import FWGL.Backend.OpenGL.GL20
import qualified FWGL.Backend.GLFW.Common as C

instance BackendIO where
        loadImage = C.loadImage
        loadTextFile = C.loadTextFile
        setup = C.setup C.ClientAPI'OpenGL 2 0 -- TODO: enable extensions
