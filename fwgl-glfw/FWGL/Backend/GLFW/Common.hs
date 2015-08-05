module FWGL.Backend.GLFW.Common (
        loadTextFile,
        loadImage,
        initBackend,
        createCanvas,
        setCanvasSize,
        setCanvasTitle,
        setCanvasResizeCallback,
        setCanvasRefreshCallback,
        popInput,
        getInput,
        drawCanvas,
        updateCanvas,
        refreshLoop,
        FWGL.Backend.GLFW.Common.getTime,
        terminateBackend,
        Canvas,
        ClientAPI(..)
) where

import Codec.Picture
import Codec.Picture.Types (promoteImage)
import Control.Concurrent
import Control.Monad
import Control.Exception.Base (catch)
import qualified Data.HashMap.Strict as H
import Data.IORef
import Data.Vector.Storable (unsafeWith)
import Foreign.Ptr
import FRP.Yampa
import FWGL.Backend.GLES hiding (Image)
import FWGL.Input as Input
import Graphics.UI.GLFW as GLFW

loadImage :: (Num sz, Num sz') => String
          -> (((sz', sz', Ptr ()), sz, sz) -> IO a) -> IO a
loadImage path c = do eimg <- readImage path
                      case eimg of
                            Left err -> error err
                            Right img ->
                                    case convert img of
                                        Image w h v -> unsafeWith v $
                                                \p -> c ( ( fromIntegral w
                                                          , fromIntegral h
                                                          , castPtr p)
                                                        , fromIntegral w
                                                        , fromIntegral h)
        where convert :: DynamicImage -> Image PixelRGBA8
              convert (ImageRGBA8 img) = img
              convert (ImageRGB8 img) = promoteImage img
              convert (ImageYA8 img) = promoteImage img
              convert (ImageY8 img) = promoteImage img
              convert _ = error "Unsupported image format."

loadTextFile :: String -> (Either String String -> IO ()) -> IO ()
loadTextFile fname handler = (>> return ()) . forkIO $
        catch (fmap (\s -> s `seq` Right s) $ readFile fname)
              (\e -> return (Left $ show (e :: IOError))) >>= handler

data Canvas = Canvas GLFW.Window
                     (IORef (H.HashMap InputEvent [EventData]))
                     (IORef (Int -> Int -> IO ()))
                     (IORef (IO ()))

initBackend :: IO ()
initBackend = GLFW.init >> setTime 0

createCanvas :: ClientAPI -> Int -> Int
             -> String -> Int -> Int -> IO (Canvas, Int, Int)
createCanvas clientAPI maj min title w h =
        do windowHint $ WindowHint'ClientAPI clientAPI
           windowHint $ WindowHint'ContextVersionMajor maj
           windowHint $ WindowHint'ContextVersionMinor min
           Just win <- createWindow w h title Nothing Nothing

           resizeRef <- newIORef $ \_ _ -> return ()
           refreshRef <- newIORef $ return ()
           eventsRef <- newIORef $ H.singleton Resize [
                                        emptyEventData {
                                            dataFramebufferSize = Just (w, h)
                                        }]


           setKeyCallback win . Just . const $ keyCallback eventsRef
           setMouseButtonCallback win . Just $ mouseCallback eventsRef
           setCursorPosCallback win . Just . const $ cursorCallback eventsRef
           setWindowRefreshCallback win . Just $
                   refreshCallback refreshRef
           setFramebufferSizeCallback win . Just . const $
                   resizeCallback eventsRef resizeRef

           return (Canvas win eventsRef resizeRef refreshRef, w, h)

        where keyCallback events key _ keyState _ = modifyIORef' events $
                      case keyState of
                              KeyState'Pressed -> insertEvent KeyDown keyData
                              KeyState'Released -> insertEvent KeyUp keyData
                              _ -> id
                where keyData = emptyEventData {
                                        dataKey = Just $ toKey key
                                }

              mouseCallback events win mb mbState _ = do
                      pos <- fmap convertCursorPos $ getCursorPos win
                      modifyIORef' events $
                        case mbState of
                                MouseButtonState'Pressed ->
                                        insertEvent MouseDown $ keyData pos
                                MouseButtonState'Released ->
                                        insertEvent MouseUp $ keyData pos
                where keyData p = emptyEventData {
                                        dataButton = Just $ toMouseButton mb,
                                        dataPointer = Just p
                                }

              cursorCallback events x y = modifyIORef' events $
                      insertEvent MouseMove $ emptyEventData {
                                dataPointer = Just $ convertCursorPos (x, y)
                        }

              resizeCallback events resizeRef x y =
                      do callback <- readIORef resizeRef
                         callback x y
                         modifyIORef' events $
                                insertEvent Resize $ emptyEventData {
                                        dataFramebufferSize = Just $ (x, y)
                                }

              refreshCallback refreshRef win =
                      do makeContextCurrent $ Just win
                         join $ readIORef refreshRef


              convertCursorPos (x, y) = (floor x, floor y)

              insertEvent e = H.insertWith (flip (++)) e . return

              emptyEventData = EventData {
                                dataFramebufferSize = Nothing,
                                dataPointer = Nothing,
                                dataButton = Nothing,
                                dataKey = Nothing }

setCanvasSize :: Int -> Int -> Canvas -> IO ()
setCanvasSize w h (Canvas win _ _ _) = setWindowSize win w h

setCanvasTitle :: String -> Canvas -> IO ()
setCanvasTitle str (Canvas win _ _ _) = setWindowTitle win str

setCanvasResizeCallback :: (Int -> Int -> IO ()) -> Canvas -> IO ()
setCanvasResizeCallback callback (Canvas _ _ ref _) = writeIORef ref callback

setCanvasRefreshCallback :: IO () -> Canvas -> IO ()
setCanvasRefreshCallback callback (Canvas _ _ _ ref) = writeIORef ref callback

popInput :: a -> Canvas -> IO (Input a)
popInput c canvas@(Canvas _ events _ _) = do i <- getInput c canvas
                                             writeIORef events H.empty
                                             return i

getInput :: a -> Canvas -> IO (Input a)
getInput c (Canvas _ events _ _) = flip Input c <$> readIORef events

drawCanvas :: (() -> IO a) -> Bool -> Canvas -> IO a
drawCanvas act shouldSwap (Canvas win _ _ _) = do makeContextCurrent $ Just win
                                                  r <- act ()
                                                  when shouldSwap $
                                                          swapBuffers win
                                                  return r

updateCanvas :: Canvas -> IO Bool
updateCanvas (Canvas win _ _ _) = do pollEvents
                                     close <- windowShouldClose win
                                     when close $ destroyWindow win
                                     return close

refreshLoop :: Int -> Canvas -> IO ()
refreshLoop fps c@(Canvas _ _ _ ref) = do closed <- updateCanvas c
                                          join $ readIORef ref
                                          threadDelay $ div 1000000 fps
                                          unless closed $ refreshLoop fps c

getTime :: IO Double
getTime = do Just t <- GLFW.getTime
             return t

terminateBackend :: IO ()
terminateBackend = terminate

toMouseButton :: GLFW.MouseButton -> Input.MouseButton
toMouseButton MouseButton'1 = MouseLeft
toMouseButton MouseButton'3 = MouseMiddle
toMouseButton MouseButton'2 = MouseRight

toKey :: GLFW.Key -> Input.Key
toKey Key'A = KeyA
toKey Key'B = KeyB
toKey Key'C = KeyC
toKey Key'D = KeyD
toKey Key'E = KeyE
toKey Key'F = KeyF
toKey Key'G = KeyG
toKey Key'H = KeyH
toKey Key'I = KeyI
toKey Key'J = KeyJ
toKey Key'K = KeyK
toKey Key'L = KeyL
toKey Key'M = KeyM
toKey Key'N = KeyN
toKey Key'O = KeyO
toKey Key'P = KeyP
toKey Key'Q = KeyQ
toKey Key'R = KeyR
toKey Key'S = KeyS
toKey Key'T = KeyT
toKey Key'U = KeyU
toKey Key'V = KeyV
toKey Key'W = KeyW
toKey Key'X = KeyX
toKey Key'Y = KeyY
toKey Key'Z = KeyZ
toKey Key'0 = Key0
toKey Key'1 = Key1
toKey Key'2 = Key2
toKey Key'3 = Key3
toKey Key'4 = Key4
toKey Key'5 = Key5
toKey Key'6 = Key6
toKey Key'7 = Key7
toKey Key'8 = Key8
toKey Key'9 = Key9
toKey Key'Space = KeySpace
toKey Key'Enter = KeyEnter
toKey Key'Tab = KeyTab
toKey Key'Escape = KeyEsc
toKey Key'Backspace = KeyBackspace
toKey Key'LeftShift = KeyShift -- TODO: different shifts
toKey Key'RightShift = KeyShift
toKey Key'LeftControl = KeyControl -- TODO: //
toKey Key'RightControl = KeyControl
toKey Key'LeftAlt = KeyAlt -- TODO: //
toKey Key'RightAlt = KeyAlt
toKey Key'CapsLock = KeyCapsLock
toKey Key'NumLock = KeyNumLock
toKey Key'Left = KeyArrowLeft
toKey Key'Up = KeyArrowUp
toKey Key'Right = KeyArrowRight
toKey Key'Down = KeyArrowDown
toKey Key'Insert = KeyIns
toKey Key'Delete = KeyDel
toKey Key'Home = KeyHome
toKey Key'End = KeyEnd
toKey Key'PageUp = KeyPgUp
toKey Key'PageDown = KeyPgDown
toKey Key'F1 = KeyF1
toKey Key'F2 = KeyF2
toKey Key'F3 = KeyF3
toKey Key'F4 = KeyF4
toKey Key'F5 = KeyF5
toKey Key'F6 = KeyF6
toKey Key'F7 = KeyF7
toKey Key'F8 = KeyF8
toKey Key'F9 = KeyF9
toKey Key'F10 = KeyF10
toKey Key'F11 = KeyF11
toKey Key'F12 = KeyF12
toKey Key'PadAdd = KeyPadAdd
toKey Key'PadSubtract = KeyPadSub
toKey Key'PadMultiply = KeyPadMul
toKey Key'PadDivide = KeyPadDiv
toKey Key'PadEnter = KeyPadEnter
toKey Key'PadDecimal = KeyPadDot
toKey Key'Pad0 = KeyPad0
toKey Key'Pad1 = KeyPad1
toKey Key'Pad2 = KeyPad2
toKey Key'Pad3 = KeyPad3
toKey Key'Pad4 = KeyPad4
toKey Key'Pad5 = KeyPad5
toKey Key'Pad6 = KeyPad6
toKey Key'Pad7 = KeyPad7
toKey Key'Pad8 = KeyPad8
toKey Key'Pad9 = KeyPad9
toKey _ = KeyUnknown
