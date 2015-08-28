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
        -- * Raw
        Input(..),
        InputEvent(..),
        EventData(..),
) where

import Data.Maybe
import Data.Hashable
import qualified Data.HashMap.Strict as H
import Debug.Trace
import FWGL.Key
import FRP.Yampa

-- | An event.
data InputEvent = KeyUp | KeyDown | MouseUp | MouseDown | MouseMove | Resize
                  deriving (Show, Eq, Enum)

-- | The data carried by an event. They're all together in the same structure
-- because this is how it works in JavaScript.
data EventData = EventData {
        dataFramebufferSize :: Maybe (Int, Int),
        dataPointer :: Maybe (Int, Int),
        dataButton :: Maybe MouseButton,
        dataKey :: Maybe Key,
        dataTime :: Double -- ^ The unit of time is unspecified, this is only
                           -- used to determine the sequence of different
                           -- events.
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
key k = sscan upDown NoEvent <<<
        evEdgeTime KeyUp (isKey k) &&& evEdgeTime KeyDown (isKey k)

-- | Mouse press.
mouseDown :: MouseButton -> SF (Input a) (Event (Int, Int))
mouseDown b = evEdgePointer MouseDown (isButton b) >>^ fmap snd

-- | Mouse release.
mouseUp :: MouseButton -> SF (Input a) (Event (Int, Int))
mouseUp b = evEdgePointer MouseUp (isButton b) >>^ fmap snd

-- | Mouse down.
mouse :: MouseButton -> SF (Input a) (Event (Int, Int))
mouse b = sscan upDown NoEvent <<< evEdgePointer MouseUp (isButton b) &&&
                                   evEdgePointer MouseDown (isButton b)

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

-- TODO: remove Show a
upDown :: Show a => Event a -> (Event (Double, a), Event (Double, a)) -> Event a
upDown _ (NoEvent, Event (_, x)) = trace "noEv" $ Event x
upDown _ (Event _, NoEvent) = NoEvent
upDown _ (Event (t, _), Event (t', x)) | t' > t = traceShow (t, t') $ Event x
                                       | otherwise = noEvent
upDown e _ = e

isKey :: Key -> EventData -> Bool
isKey k ed = dataKey ed == Just k

isButton :: MouseButton -> EventData -> Bool
isButton btn evData = dataButton evData == Just btn

evSearch :: InputEvent -> (EventData -> Bool) -> SF (Input a) (Event EventData)
evSearch ev bP = arr $ \inp -> case H.lookup ev $ inputEvents inp of
                                    Just bs -> eventHead $ filter bP bs
                                    Nothing -> NoEvent

evEdge :: InputEvent -> (EventData -> Bool) -> SF (Input a) (Event ())
evEdge ev bP = (evSearch ev bP >>^ isEvent) >>> edge

evEdgeData :: InputEvent -> (EventData -> Bool)
           -> SF (Input a) (Event EventData)
evEdgeData ev bP = (evSearch ev bP >>^ event Nothing Just) >>> edgeJust

evEdgeTime :: InputEvent -> (EventData -> Bool)
           -> SF (Input a) (Event (Double, ()))
evEdgeTime ev bP = evEdgeData ev bP >>^ fmap (flip (,) () . dataTime)

evEdgePointer :: InputEvent -> (EventData -> Bool)
              -> SF (Input a) (Event (Double, (Int, Int)))
evEdgePointer ev bP = evEdgeData ev bP >>^ \med -> 
                        case med of
                             Event ed ->
                                case dataPointer ed of
                                     Just ptr -> Event (dataTime ed, ptr)
                                     Nothing -> NoEvent
                             NoEvent -> NoEvent

evPointer :: InputEvent -> (EventData -> Bool)
          -> SF (Input a) (Event (Int, Int))
evPointer ev bP = evSearch ev bP >>^ \e ->
                        case e of
                             Event ed -> case dataPointer ed of
                                              Just ptr -> Event ptr
                                              _ -> NoEvent
                             NoEvent -> NoEvent

eventHead :: [a] -> Event a
eventHead [] = NoEvent
eventHead (x : _) = Event x
