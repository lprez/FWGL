-- old code

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
                                        <$> (do w <- prop "clientWidth" j
                                                h <- prop "clientHeight" j
                                                return $ (,) <$> w <*> h)
                                        <*> (do x <- prop "clientX" d
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
getKey 65 = KeyA
getKey 66 = KeyB
getKey 67 = KeyC
getKey 68 = KeyD
getKey 69 = KeyE
getKey 70 = KeyF
getKey 71 = KeyG
getKey 72 = KeyH
getKey 73 = KeyI
getKey 74 = KeyJ
getKey 75 = KeyK
getKey 76 = KeyL
getKey 77 = KeyM
getKey 78 = KeyN
getKey 79 = KeyO
getKey 80 = KeyP
getKey 81 = KeyQ
getKey 82 = KeyR
getKey 83 = KeyS
getKey 84 = KeyT
getKey 85 = KeyU
getKey 86 = KeyV
getKey 87 = KeyW
getKey 88 = KeyX
getKey 89 = KeyY
getKey 90 = KeyZ
getKey 97 = KeyA
getKey 98 = KeyB
getKey 99 = KeyC
getKey 100 = KeyD
getKey 101 = KeyE
getKey 102 = KeyF
getKey 103 = KeyG
getKey 104 = KeyH
getKey 105 = KeyI
getKey 106 = KeyJ
getKey 107 = KeyK
getKey 108 = KeyL
getKey 109 = KeyM
getKey 110 = KeyN
getKey 111 = KeyO
getKey 112 = KeyP
getKey 113 = KeyQ
getKey 114 = KeyR
getKey 115 = KeyS
getKey 116 = KeyT
getKey 117 = KeyU
getKey 118 = KeyV
getKey 119 = KeyW
getKey 120 = KeyX
getKey 121 = KeyY
getKey 122 = KeyZ
getKey 48 = Key0
getKey 49 = Key1
getKey 50 = Key2
getKey 51 = Key3
getKey 52 = Key4
getKey 53 = Key5
getKey 54 = Key6
getKey 55 = Key7
getKey 56 = Key8
getKey 57 = Key9
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
getKey _ = KeyUnknown
-- TODO: letters, numbers
-- TODO: fix overlapping patterns
