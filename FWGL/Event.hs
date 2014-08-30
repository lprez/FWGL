module FWGL.Event (
        module JavaScript.Key,
        MouseButton(..),
        keyUp,
        keyDown,
        key,
        mouseDown,
        mouseUp,
        mouse,
        click,
        doubleClick,
        pointer
) where

import FWGL.Internal

import JavaScript.Event hiding (Event)
import JavaScript.Key

import FRP.Yampa

keyUp :: KeyCode a => a -> SF Input (Event ())
keyUp k = evEdge (== KeyUp) $ isKey k

keyDown :: KeyCode a => a -> SF Input (Event ())
keyDown k = evEdge (== KeyDown) $ isKey k

key :: KeyCode a => a -> SF Input (Event ())
key k = sscan upDown NoEvent <<< keyUp k &&& keyDown k

mouseDown :: MouseButton -> SF Input (Event (Int, Int))
mouseDown b = evPointer MouseDown $ \d -> button d == Just b

mouseUp :: MouseButton -> SF Input (Event (Int, Int))
mouseUp b = evPointer MouseUp $ \d -> button d == Just b

mouse :: MouseButton -> SF Input (Event (Int, Int))
mouse b = sscan upDown NoEvent <<< mouseUp b &&& mouseDown b

click :: SF Input (Event (Int, Int))
click = mouseDown MouseLeft

doubleClick :: SF Input (Event (Int, Int))
doubleClick = evPointer DoubleClick (const True)

pointer :: SF Input (Int, Int)
pointer = evPointer MouseMove (const True) >>> hold (0, 0)

{- keyDownLimited :: KeyCode a => Double -> a -> SF Input (Event ())

keyLimited :: KeyCode a => Double -> a -> SF Input (Event ()) -}

upDown :: Event a -> (Event a, Event a) -> Event a
upDown _ (_, Event x) = Event x
upDown (Event _) (Event _, _) = NoEvent
upDown s _ = s

evSearch :: (a -> Bool) -> (b -> Bool) -> SF [(a, b)] (Event (a, b))
evSearch aP bP = arr $ eventHead . filter (\(a, b) -> aP a && bP b)

evEdge :: (a -> Bool) -> (b -> Bool) -> SF [(a, b)] (Event ())
evEdge aP bP = evSearch aP bP >>> arr isEvent >>> edge

evPointer :: Eq a => a -> (EventData -> Bool)
          -> SF [(a, EventData)] (Event (Int, Int))
evPointer a bP = evSearch (== a) bP >>>
                 arr (\e -> case e of
                        Event (_, ed) -> case (clientX ed, clientY ed) of
                                                (Just x, Just y) -> Event (x, y)
                                                _ -> NoEvent
                        NoEvent -> NoEvent
                  )

eventHead :: [a] -> Event a
eventHead [] = NoEvent
eventHead (x : _) = Event x
