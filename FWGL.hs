module FWGL (
        module X,
        Input,
        run
) where

import FWGL.Audio as X
import FWGL.Event as X
import FWGL.Internal
import FWGL.Graphics as X
import FWGL.Graphics.Draw

import JavaScript.Event
import JavaScript.WebGL hiding (clear)

import Data.IORef
import Control.Applicative
import Control.Concurrent (threadDelay)
import FRP.Yampa
import GHCJS.Foreign
import GHCJS.Types

foreign import javascript unsafe "document.querySelector($1)"
        query :: JSString -> IO (JSRef a)

run :: String -> SF Input (Scene, Audio) -> IO ()
run q sigf = do element <- query $ toJSString q
                eventSrc <- source handledEvents element
                ctx <- getCtx element
                drawStateRef <- drawInit ctx 640 480 >>= newIORef -- chg
                reactimate (clear eventSrc)
                           (const $ sense eventSrc)
                           (const $ actuate drawStateRef)
                           sigf
                -- TODO requestanimframe, reactInit, ...
        where sense src = threadDelay 60000 >> (,) 60 . Just <$> clear src
              actuate ref (s, _) = readIORef ref >>=
                                   execDraw (drawBegin >> drawScene s) >>=
                                   writeIORef ref >> return False
              handledEvents = [ MouseUp
                              , MouseDown
                              , MouseMove
                              , DoubleClick
                              , KeyUp
                              , KeyDown ]
