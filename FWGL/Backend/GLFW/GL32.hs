{-# LANGUAGE NullaryTypeClasses, TypeFamilies #-}

module FWGL.Backend.GLFW.GL32 () where

import FWGL.Backend.IO
import FWGL.Backend.OpenGL.GL32
import qualified FWGL.Backend.GLFW.Common as C

instance BackendIO where
        loadImage = C.loadImage
        setup = C.setup C.ClientAPI'OpenGL 3 2
