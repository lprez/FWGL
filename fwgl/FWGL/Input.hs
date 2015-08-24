module FWGL.Input (
        module FWGL.Key,
        -- * FRP
        keyUp,
        keyDown,
        key,
        mouseDown,
        mouseUp,
        mouse,
        click,
        mouseMove,
        pointer,
        resize,
        size,
        custom,
        -- * IO
        Input(..),
        InputEvent(..),
        EventData(..),
) where

import Data.Maybe
import Data.Hashable
import qualified Data.HashMap.Strict as H
import FWGL.Key
import FRP.Yampa

-- | An event.
data InputEvent = KeyUp | KeyDown | MouseUp | MouseDown | MouseMove | Resize
                  deriving (Show, Eq, Enum)

-- | The data carried by an event.
data EventData = EventData {
        dataFramebufferSize :: Maybe (Int, Int),
        dataPointer :: Maybe (Int, Int),
        dataButton :: Maybe MouseButton,
        dataKey :: Maybe Key
}

-- | The general input.
data Input a = Input {
        inputEvents :: H.HashMap InputEvent [EventData],
        inputCustom :: a
}

instance Hashable InputEvent where
        hashWithSalt salt = hashWithSalt salt . fromEnum

-- | Keyboard release.
keyUp :: Key -> SF (Input a) (Event ())
keyUp k = evEdge KeyUp $ isKey k

-- | Keyboard press.
keyDown :: Key -> SF (Input a) (Event ())
keyDown k = evEdge KeyDown $ isKey k

-- | Keyboard down.
key :: Key -> SF (Input a) (Event ())
key k = sscan upDown NoEvent <<< keyUp k &&& keyDown k

-- | Mouse press.
mouseDown :: MouseButton -> SF (Input a) (Event (Int, Int))
mouseDown b = evPointer MouseDown $ \d -> dataButton d == Just b

-- | Mouse release.
mouseUp :: MouseButton -> SF (Input a) (Event (Int, Int))
mouseUp b = evPointer MouseUp $ \d -> dataButton d == Just b

-- | Mouse down.
mouse :: MouseButton -> SF (Input a) (Event (Int, Int))
mouse b = sscan upDown NoEvent <<< mouseUp b &&& mouseDown b

-- | Left click.
click :: SF (Input a) (Event (Int, Int))
click = mouseDown MouseLeft

-- | Mouse move.
mouseMove :: SF (Input a) (Event (Int, Int))
mouseMove = evPointer MouseMove (const True)

-- | Pointer location in pixels.
pointer :: SF (Input a) (Int, Int)
pointer = mouseMove >>> hold (0, 0)

-- | Window/framebuffer/canvas/etc. resize.
resize :: SF (Input a) (Event (Int, Int))
resize = evSearch Resize (isJust . dataFramebufferSize) >>^
         fmap (fromJust . dataFramebufferSize)

-- | Window/framebuffer/canvas size.
size :: SF (Input a) (Int, Int)
size = resize >>> hold (0, 0)

-- | Custom input.
custom :: SF (Input a) a
custom = arr inputCustom

{- keyDownLimited :: KeyCode a => Double -> a -> SF Input (Event ())

keyLimited :: KeyCode a => Double -> a -> SF Input (Event ()) -}

upDown :: Event a -> (Event a, Event a) -> Event a
upDown _ (NoEvent, Event x) = Event x
upDown (Event _) (Event _, NoEvent) = NoEvent
upDown s _ = s

isKey :: Key -> EventData -> Bool
isKey k ed = case dataKey ed of
                Just k' -> k == k'
                Nothing -> False

evSearch :: InputEvent -> (EventData -> Bool) -> SF (Input a) (Event EventData)
evSearch ev bP = arr $ \ inp -> case H.lookup ev $ inputEvents inp of
                                        Just bs -> eventHead $ filter bP bs
                                        Nothing -> NoEvent

evEdge :: InputEvent -> (EventData -> Bool) -> SF (Input a) (Event ())
evEdge ev bP = evSearch ev bP >>> arr isEvent >>> edge

evPointer :: InputEvent -> (EventData -> Bool)
          -> SF (Input a) (Event (Int, Int))
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
