{-# LANGUAGE NullaryTypeClasses, TypeFamilies #-}

module FWGL.Backend.GLFW.GL20 (createCanvas') where

import FWGL.Backend.IO
import FWGL.Backend.OpenGL.GL20
import qualified FWGL.Backend.GLFW.Common as C

createCanvas' :: String         -- ^ Window title
              -> Int            -- ^ Window width
              -> Int            -- ^ Window height
              -> IO (C.Canvas, Int, Int)
createCanvas' = C.createCanvas C.ClientAPI'OpenGL 2 0

instance BackendIO where
        type Canvas = C.Canvas

        loadImage = C.loadImage
        loadTextFile = C.loadTextFile
        initBackend = C.initBackend
        createCanvas = createCanvas' "" 640 480
        setCanvasSize = C.setCanvasSize
        setCanvasTitle = C.setCanvasTitle
        setCanvasResizeCallback = C.setCanvasResizeCallback
        setCanvasRefreshCallback = C.setCanvasRefreshCallback
        popInput = C.popInput
        getInput = C.getInput
        drawCanvas = C.drawCanvas
        refreshLoop = C.refreshLoop
        updateCanvas = C.updateCanvas
        getTime = C.getTime
        terminateBackend = C.terminateBackend
