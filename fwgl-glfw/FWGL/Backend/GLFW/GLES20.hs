{-# LANGUAGE NullaryTypeClasses, TypeFamilies #-}

module FWGL.Backend.GLFW.GLES20 () where

import FWGL.Backend.IO
import FWGL.Backend.OpenGL.GLES20
import qualified FWGL.Backend.GLFW.Common as C

instance BackendIO where
        loadImage = C.loadImage
        loadTextFile = C.loadTextFile
        setup = C.setup C.ClientAPI'OpenGLES 2 0
