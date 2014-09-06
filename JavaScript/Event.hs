module JavaScript.Event (
        Source,
        MouseButton(..),
        Event(..),
        EventData(..),
        source,
        addEvent,
        addEvents,
        events,
        clear,
        isKey,
        isKeyI,
        mouseEvents,
        keyboardEvents,
        allEvents
) where

import Control.Applicative
import Data.Char (toLower, toUpper)
import qualified Data.Hashable as H
import qualified Data.HashMap.Strict as H
import Data.IORef

import GHCJS.Foreign
import GHCJS.Marshal
import GHCJS.Types

import JavaScript.Key

data Source = Source {
        element :: JSRef (),
        eventMap :: IORef (H.HashMap Event [EventData])
}

data MouseButton = MouseLeft | MouseMiddle | MouseRight deriving (Show, Eq)

data Event = Click
           | DoubleClick
           | MouseDown
           | MouseMove
           | MouseOver
           | MouseOut
           | MouseUp
           | KeyDown
           | KeyUp
           | KeyPress
           | Abort
           | Error
           | Load
           | Resize
           | Scroll
           | Unload
           | Blur
           | Change
           | Focus
           | Reset
           | Select
           | Submit
           | TouchStart
           | TouchMove
           | TouchEnd
           | Other String
           deriving (Eq, Show)

data EventData = EventData {
        button :: Maybe MouseButton,
        keyCode :: Maybe Int,
        clientX :: Maybe Int,
        clientY :: Maybe Int,
        altKey :: Maybe Bool,
        ctrlKey :: Maybe Bool,
        metaKey :: Maybe Bool,
        shiftKey :: Maybe Bool
} deriving (Show)

instance H.Hashable Event where
        hashWithSalt salt = H.hashWithSalt salt . eventName
        hash = H.hash . eventName

source :: [Event] -> JSRef a -> IO Source
source es j = do
        s <- Source (castRef j) <$> newIORef H.empty
        addEvents es s
        return s

events :: Source -> IO (H.HashMap Event [EventData])
events (Source _ c) = readIORef c

clear :: Source -> IO (H.HashMap Event [EventData])
clear (Source _ c) = readIORef c <* writeIORef c H.empty

addEvents :: [Event] -> Source -> IO ()
addEvents es s = mapM_ (flip addEvent s) es

addEvent :: Event -> Source -> IO ()
addEvent e (Source j c) = asyncCallback1 AlwaysRetain handler >>=
                          addHandler j (toJSString $ eventName e)
        where 
                prop p d =  getProp p d >>= fromJSRef
                handler d = do
                        print e
                        eventData <- EventData
                                        <$> ((getButton <$>) <$> prop "button" d)
                                        <*> prop "keyCode" d
                                        <*> prop "clientX" d
                                        <*> prop "clientY" d
                                        <*> prop "altKey" d
                                        <*> prop "ctrlKey" d
                                        <*> prop "metaKey" d
                                        <*> prop "shiftKey" d
                        modifyIORef c $ H.insertWith (flip (++)) e [ eventData ]

isKey :: KeyCode a => a -> EventData -> Bool
isKey x e = case keyCode e of
                Just i -> i == toKeyCode x
                Nothing -> False

isKeyI :: Char -> EventData -> Bool
isKeyI c e = isKey (toLower c) e || isKey (toUpper c) e

mouseEvents :: [Event]
mouseEvents = [ Click, DoubleClick, MouseDown, MouseMove
              , MouseOver, MouseOut, MouseUp ]

keyboardEvents :: [Event]
keyboardEvents = [ KeyDown, KeyUp, KeyPress ]

allEvents :: [Event]
allEvents = [ Click, DoubleClick, MouseDown, MouseMove, MouseOver, MouseOut
            , MouseUp, KeyDown, KeyUp, KeyPress, Abort, Error, Load, Resize
            , Scroll, Unload, Blur, Change, Focus, Reset, Select, Submit
            , TouchStart, TouchMove, TouchEnd ]

eventName :: Event -> String
eventName (Other s) = s
eventName DoubleClick = "dblclick"
eventName s = map toLower . show $ s

getButton :: Int -> MouseButton
getButton 0 = MouseLeft
getButton 1 = MouseMiddle
getButton 2 = MouseRight

foreign import javascript unsafe "$1.addEventListener($2, $3)"
        addHandler :: JSRef a -> JSString -> JSFun (JSRef b -> IO ()) -> IO ()
