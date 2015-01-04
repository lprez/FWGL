module FWGL.Input (
        module FWGL.Key,
        Input(..),
        InputEvent(..),
        EventData(..),
        keyUp,
        keyDown,
        key,
        mouseDown,
        mouseUp,
        mouse,
        click,
        pointer,
        resize,
        size
) where

import Data.Maybe
import Data.Hashable
import qualified Data.HashMap.Strict as H
import FWGL.Key
import FRP.Yampa

data InputEvent = KeyUp | KeyDown | MouseUp | MouseDown | MouseMove | Resize
                  deriving (Show, Eq, Enum)

data EventData = EventData {
        dataFramebufferSize :: Maybe (Int, Int),
        dataPointer :: Maybe (Int, Int),
        dataButton :: Maybe MouseButton,
        dataKey :: Maybe Key
}

data Input = Input {
        inputEvents :: H.HashMap InputEvent [EventData]
}

instance Hashable InputEvent where
        hashWithSalt salt = hashWithSalt salt . fromEnum

keyUp :: Key -> SF Input (Event ())
keyUp k = evEdge KeyUp $ isKey k

keyDown :: Key -> SF Input (Event ())
keyDown k = evEdge KeyDown $ isKey k

key :: Key -> SF Input (Event ())
key k = sscan upDown NoEvent <<< keyUp k &&& keyDown k

mouseDown :: MouseButton -> SF Input (Event (Int, Int))
mouseDown b = evPointer MouseDown $ \d -> dataButton d == Just b

mouseUp :: MouseButton -> SF Input (Event (Int, Int))
mouseUp b = evPointer MouseUp $ \d -> dataButton d == Just b

mouse :: MouseButton -> SF Input (Event (Int, Int))
mouse b = sscan upDown NoEvent <<< mouseUp b &&& mouseDown b

click :: SF Input (Event (Int, Int))
click = mouseDown MouseLeft

-- | Pointer location in pixels.
pointer :: SF Input (Int, Int)
pointer = evPointer MouseMove (const True) >>> hold (0, 0)

resize :: SF Input (Event (Int, Int))
resize = evSearch Resize (isJust . dataFramebufferSize) >>^
         fmap (fromJust . dataFramebufferSize)

size :: SF Input (Int, Int)
size = resize >>> hold (0, 0)

{- keyDownLimited :: KeyCode a => Double -> a -> SF Input (Event ())

keyLimited :: KeyCode a => Double -> a -> SF Input (Event ()) -}

upDown :: Event a -> (Event a, Event a) -> Event a
upDown _ (_, Event x) = Event x
upDown (Event _) (Event _, _) = NoEvent
upDown s _ = s

isKey :: Key -> EventData -> Bool
isKey k ed = case dataKey ed of
                Just k' -> k == k'
                Nothing -> False

evSearch :: InputEvent -> (EventData -> Bool) -> SF Input (Event EventData)
evSearch ev bP = arr $ \ inp -> case H.lookup ev $ inputEvents inp of
                                        Just bs -> eventHead $ filter bP bs
                                        Nothing -> NoEvent

evEdge :: InputEvent -> (EventData -> Bool) -> SF Input (Event ())
evEdge ev bP = evSearch ev bP >>> arr isEvent >>> edge

evPointer :: InputEvent -> (EventData -> Bool) -> SF Input (Event (Int, Int))
evPointer ev bP = evSearch ev bP >>>
                 arr (\ e -> case e of
                        Event ed -> case dataPointer ed of
                                        Just (x, y) -> Event (x, y)
                                        _ -> NoEvent
                        NoEvent -> NoEvent
                  )

eventHead :: [a] -> Event a
eventHead [] = NoEvent
eventHead (x : _) = Event x
