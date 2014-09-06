module FWGL (
        module FWGL.Audio,
        module FWGL.Event,
        module FWGL.Graphics,
        Input,
        run
) where

import FWGL.Audio
import FWGL.Event
import FWGL.Internal
import FWGL.Graphics
import FWGL.Graphics.Draw

import JavaScript.Event
import JavaScript.WebGL hiding (clear)

import Data.IORef
import Control.Applicative
import Control.Concurrent (threadDelay)
import FRP.Yampa
import GHCJS.Foreign
import GHCJS.Marshal
import GHCJS.Types

foreign import javascript unsafe "document.querySelector($1)"
        query :: JSString -> IO (JSRef a)

foreign import javascript unsafe "$2.getAttribute($1)"
        getAttributeRaw :: JSString -> JSRef a -> IO JSString

foreign import javascript unsafe "window.requestAnimationFrame($1)"
        requestAnimationFrame :: JSFun (JSRef Double -> IO ()) -> IO ()

{-
foreign import javascript unsafe "document.body.onload=$1"
        onBodyLoadRaw :: JSFun (JSRef a -> IO ()) -> IO ()

onBodyLoad :: IO () -> IO ()
onBodyLoad act = asyncCallback1 AlwaysRetain (const act) >>= onBodyLoadRaw
-}

getAttribute :: String -> JSRef a -> IO String
getAttribute attr e = fromJSString <$> getAttributeRaw (toJSString attr) e

run :: String                   -- ^ Selector for the canvas element. See <https://developer.mozilla.org/en-US/docs/Web/API/document.querySelector querySelector>
    -> SF Input (Scene, Audio)  -- ^ Main signal
    -> IO ()
run q sigf = do element <- query $ toJSString q
                eventSrc <- source handledEvents element
                ctx <- getCtx element
                w <- read <$> getAttribute "width" element
                h <- read <$> getAttribute "height" element
                drawStateRef <- drawInit ctx w h >>= newIORef
                reactStateRef <- reactInit (clear eventSrc)
                                           (\ _ _ -> actuate drawStateRef)
                                           sigf
                onFrame $ frame reactStateRef eventSrc Nothing
        where frame rsf src last crf = do events <- clear src
                                          (Just cur) <- fromJSRef crf
                                          let tm = case last of
                                                        Just l -> cur - l
                                                        Nothing -> 0
                                          react rsf (tm, Just events)
                                          onFrame $ frame rsf src (Just cur)

              onFrame handler = asyncCallback1 AlwaysRetain handler -- try NeverRetain
                                >>= requestAnimationFrame
                             
              actuate ref (s, _) = readIORef ref >>=
                                   execDraw (drawBegin >> drawScene s) >>=
                                   writeIORef ref >> return False
              handledEvents = [ MouseUp
                              , MouseDown
                              , MouseMove
                              , DoubleClick
                              , KeyUp
                              , KeyDown
                              , TouchStart
                              , TouchMove
                              , TouchEnd ]
