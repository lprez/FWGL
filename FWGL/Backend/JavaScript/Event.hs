-- TODO: rewrite everything

module FWGL.Backend.JavaScript.Event (
        InputEvent(..),
        Source,
        source,
        addEvent,
        addEvents,
        events,
        clear
) where

import Control.Applicative
import Data.Char (toLower, toUpper)
import qualified Data.Hashable as H
import qualified Data.HashMap.Strict as H
import Data.IORef

import GHCJS.Foreign
import GHCJS.Marshal
import GHCJS.Types

import FWGL.Input

type Event = InputEvent

data Source = Source {
        element :: JSRef (),
        eventMap :: IORef (H.HashMap Event [EventData])
}

source :: [Event] -> JSRef a -> IO Source
source es j = do
        s <- Source (castRef j) <$> newIORef H.empty
        addEvents es s
        return s

events :: Source -> IO (H.HashMap Event [EventData])
events (Source _ c) = readIORef c

clear :: Source -> IO (H.HashMap Event [EventData])
clear (Source _ c) = atomicModifyIORef' c $ \m -> (H.empty, m)

addEvents :: [Event] -> Source -> IO ()
addEvents es s = mapM_ (flip addEvent s) es

addEvent :: Event -> Source -> IO ()
addEvent e (Source j c) = asyncCallback1 NeverRetain handler >>=
                          addHandler j (toJSString $ eventName e)
        where 
                prop p d =  getProp p d >>= fromJSRef
                handler d = do
                        eventData <- EventData
                                        <$> (do x <- prop "clientX" d
                                                y <- prop "clientY" d
                                                return $ (,) <$> x <*> y)
                                        <*> ((getButton <$>) <$> prop "button" d)
                                        <*> ((getKey <$>) <$> prop "keyCode" d)
                        modifyIORef c $ H.insertWith (flip (++)) e [ eventData ]

eventName :: Event -> String
-- eventName (Other s) = s
-- eventName DoubleClick = "dblclick"
eventName s = map toLower . show $ s

getButton :: Int -> MouseButton
getButton 0 = MouseLeft
getButton 1 = MouseMiddle
getButton 2 = MouseRight

foreign import javascript unsafe "$1.addEventListener($2, $3)"
        addHandler :: JSRef a -> JSString -> JSFun (JSRef b -> IO ()) -> IO ()

getKey :: Int -> Key
getKey 32 = KeySpace
getKey 13 = KeyEnter
getKey 9 = KeyTab
getKey 27 = KeyEsc
getKey 8 = KeyBackspace
getKey 16 = KeyShift
getKey 17 = KeyControl
getKey 18 = KeyAlt
getKey 20 = KeyCapsLock
getKey 144 = KeyNumLock
getKey 37 = KeyArrowLeft
getKey 38 = KeyArrowUp
getKey 39 = KeyArrowRight
getKey 40 = KeyArrowDown
getKey 45 = KeyIns
getKey 46 = KeyDel
getKey 36 = KeyHome
getKey 35 = KeyEnd
getKey 33 = KeyPgUp
getKey 34 = KeyPgDown
getKey 112 = KeyF1
getKey 113 = KeyF2
getKey 114 = KeyF3
getKey 115 = KeyF4
getKey 116 = KeyF5
getKey 117 = KeyF6
getKey 118 = KeyF7
getKey 119 = KeyF8
getKey 120 = KeyF9
getKey 121 = KeyF10
getKey 122 = KeyF11
getKey 123 = KeyF12
getKey 46 = KeyPadDel
getKey 45 = KeyPadIns
getKey 35 = KeyPadEnd
getKey 40 = KeyPadDown
getKey 34 = KeyPadPgDown
getKey 37 = KeyPadLeft
getKey 39 = KeyPadRight
getKey 36 = KeyPadHome
getKey 38 = KeyPadUp
getKey 33 = KeyPadPgUp
getKey 107 = KeyPadAdd
getKey 109 = KeyPadSub
getKey 106 = KeyPadMul
getKey 111 = KeyPadDiv
getKey 13 = KeyPadEnter
getKey 46 = KeyPadDot
getKey 48 = KeyPad0
getKey 49 = KeyPad1
getKey 50 = KeyPad2
getKey 51 = KeyPad3
getKey 52 = KeyPad4
getKey 53 = KeyPad5
getKey 54 = KeyPad6
getKey 55 = KeyPad7
getKey 56 = KeyPad8
getKey 57 = KeyPad9
-- TODO: letters, numbers
-- TODO: fix overlapping patterns
