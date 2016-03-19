{-# LANGUAGE NullaryTypeClasses, TypeFamilies, UndecidableInstances #-}

{-| The GHCJS/WebGL backend. This just exports the instances for 'BackendIO'
    and 'GLES'.
           
    'createCanvas' doesn't really create new canvases, but uses the first <canvas> that it finds. Use createCanvas' to choose another <canvas>. -}
module FWGL.Backend.JavaScript (
        querySelector,
        createCanvas'
) where

import Control.Applicative
import Control.Concurrent
import Data.Maybe
import qualified Data.HashMap.Strict as H
import Data.IORef
import Data.Word
import FWGL.Backend
import FWGL.Backend.JavaScript.Event
import FWGL.Input
import GHCJS.Foreign
import GHCJS.Types
import GHCJS.Marshal

foreign import javascript unsafe
        "var img = new Image();                        \
        \img.src = $1;                                 \
        \img.onload = function() { return $2(img); };  "
        loadImageRaw :: JSString -> JSFun (JSRef a -> IO ()) -> IO ()

foreign import javascript unsafe
        "var xhr = new XMLHttpRequest;                  \
        \xhr.open(\"GET\", $1, true);                   \
        \xhr.onreadystatechange = function () {         \
        \        if (xhr.readyState == 4) {             \
        \               if (xhr.status == 200) {        \
        \                       $2(xhr.responseText);   \
        \               } else {                        \
        \                       $3(xhr.responseText);   \
        \               }                               \
        \       }                                       \
        \};                                             \
        \xhr.send();                                    "
         loadTextFileRaw :: JSString
                         -> JSFun (JSString -> IO ())
                         -> JSFun (JSString -> IO ())
                         -> IO ()

foreign import javascript unsafe "document.querySelector($1)"
        querySelector :: JSString -> IO (JSRef a)

foreign import javascript unsafe "$2.getAttribute($1)"
        getAttributeRaw :: JSString -> JSRef a -> IO JSString

foreign import javascript unsafe "$3.setAttribute($1, $2)"
        setAttributeRaw :: JSString -> JSString -> JSRef a -> IO ()

getAttribute :: String -> JSRef a -> IO String
getAttribute attr e = fromJSString <$> getAttributeRaw (toJSString attr) e

setAttribute :: String -> String -> JSRef a -> IO ()
setAttribute name val = setAttributeRaw (toJSString name) (toJSString val)

foreign import javascript unsafe "window.requestAnimationFrame($1)"
        requestAnimationFrame :: JSFun (JSRef Double -> IO ()) -> IO ()

foreign import javascript unsafe "$1.focus()" focus :: JSRef a -> IO ()

data Canvas = Canvas (JSRef ())
                     Source
                     (IORef (Int -> Int -> IO ()))
                     (IORef (IO ()))

createCanvas' :: JSRef a -- ^ Canvas element (you can use 'querySelector').
              -> IO (FWGL.Backend.JavaScript.Canvas, Int, Int)
createCanvas' element = 
                do eventSrc <- source handledEvents element
                   resizeCb <- newIORef $ \_ _ -> return ()
                   refreshCb <- newIORef $ return ()
                   (Just w) <- getProp "clientWidth" element >>= fromJSRef
                   (Just h) <- getProp "clientHeight" element >>= fromJSRef
                   focus element
                   return (Canvas (castRef element) eventSrc
                                  resizeCb refreshCb, w, h)

        where handledEvents = [ MouseUp, MouseDown, MouseMove
                              , KeyUp, KeyDown, Resize ]

instance BackendIO where
        type Canvas = FWGL.Backend.JavaScript.Canvas
        type BackendState = ()

        loadImage url f = asyncCallback1 NeverRetain callback
                          >>= loadImageRaw (toJSString url) 
                where callback img =
                        do (Just w) <- getProp "width" img >>= fromJSRef
                           (Just h) <- getProp "height" img >>= fromJSRef
                           f (img, w, h)

        loadTextFile url f =
                do fRight <- asyncCallback1 NeverRetain $
                                f . Right . fromJSString
                   fLeft <- asyncCallback1 NeverRetain $
                                f . Left . fromJSString
                   forkIO $ loadTextFileRaw (toJSString url) fRight fLeft
                   return ()

        initBackend = return ()

        createCanvas s _ = querySelector (toJSString s) >>= createCanvas'

        setCanvasSize w h (Canvas e src ref _) _ =
                do setAttribute "width" (show w) e
                   setAttribute "height" (show h) e
                   cb <- readIORef ref
                   forkIO $ cb w h
                   pushEvent Resize EventData {
                        dataFramebufferSize = Just $ (w, h),
                        dataPointer = Nothing,
                        dataButton = Nothing,
                        dataKey = Nothing,
                        dataTime = 0 } src

        setCanvasTitle _ _ _ = return ()

        setCanvasResizeCallback cb (Canvas _ _ ref _) _ =
                writeIORef ref cb

        setCanvasRefreshCallback cb (Canvas _ _ _ ref) _ =
                writeIORef ref cb

        popInput c (Canvas _ src _ _) _ = flip Input c <$> clear src

        getInput c (Canvas _ src _ _) _ = flip Input c <$> events src

        drawCanvas act _ _ _ = act

        -- TODO: block
        refreshLoop t c@(Canvas _ _ _ refreshRef) bs =
                do refresh <- readIORef refreshRef
                   onFrame $ \_ -> refresh >> refreshLoop t c bs
                where onFrame handler = asyncCallback1 NeverRetain handler
                                        >>= requestAnimationFrame

        getTime _ = (/ 1000) <$> now

        terminateBackend _ = return ()
