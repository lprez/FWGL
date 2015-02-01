module FWGL.Backend.GLFW.Common (
        loadImage,
        setup,
        ClientAPI(..)
) where

import Codec.Picture
import Codec.Picture.Types (promoteImage)
import Control.Concurrent
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

setup :: ClientAPI -> Int -> Int
      -> (Int -> Int -> () -> IO state)
      -> (out -> () -> state -> IO state)
      -> SF Input out
      -> IO ()
setup clientAPI maj min initState draw sigf =
        do GLFW.init -- TODO: checks
           windowHint $ WindowHint'ClientAPI clientAPI
           windowHint $ WindowHint'ContextVersionMajor maj
           windowHint $ WindowHint'ContextVersionMinor min
           Just win <- createWindow 640 480 "" Nothing Nothing -- TODO: custom size, title
           makeContextCurrent $ Just win
           (w, h) <- getFramebufferSize win

           eventsRef <- newMVar H.empty
           drawStateRef <- initState w h () >>= newIORef
           reactStateRef <- reactInit (return $ initInput w h)
                                      (\_ _ -> actuate win drawStateRef)
                                      sigf
           setTime 0

           setKeyCallback win $ Just . const $ addKeyEvent eventsRef
           setMouseButtonCallback win . Just $ addMouseEvent eventsRef
           setCursorPosCallback win $ Just . const $ addCursorPos eventsRef

           setFramebufferSizeCallback win $ Just . const $
                   addFramebufferResize eventsRef
           setWindowRefreshCallback win $ Just . const $
                   refresh eventsRef reactStateRef

           let loop = do pollEvents
                         refresh eventsRef reactStateRef
                         close <- windowShouldClose win
                         if close
                                then do destroyWindow win
                                        terminate
                                else threadDelay 16000 >> loop
           loop
        where refresh er rsf = do (Just tm) <- getTime
                                  modifyMVar_ er $ \inp -> do
                                        react rsf (tm * 1000, Just $ Input inp)
                                        return H.empty
                                  setTime 0

              actuate win stateRef out = do newState <- readIORef stateRef
                                                        >>= draw out ()
                                            swapBuffers win
                                            writeIORef stateRef newState
                                            return False

              addKeyEvent events key _ keyState _ = modifyEvents events $
                      case keyState of
                              KeyState'Pressed -> insertEvent KeyDown keyData
                              KeyState'Released -> insertEvent KeyUp keyData
                              _ -> id
                where keyData = emptyEventData {
                                        dataKey = Just $ toKey key
                                }

              addMouseEvent events win mb mbState _ = do
                      pos <- fmap convertCursorPos $ getCursorPos win
                      modifyEvents events $
                        case mbState of
                                MouseButtonState'Pressed ->
                                        insertEvent MouseDown $ keyData pos
                                MouseButtonState'Released ->
                                        insertEvent MouseUp $ keyData pos
                where keyData p = emptyEventData {
                                        dataButton = Just $ toMouseButton mb,
                                        dataPointer = Just p
                                }

              addCursorPos events x y = modifyEvents events $
                      insertEvent MouseMove $ emptyEventData {
                                dataPointer = Just $ convertCursorPos (x, y)
                        }

              addFramebufferResize events x y = modifyEvents events $
                      insertEvent Resize $ emptyEventData {
                                dataFramebufferSize = Just $ (x, y)
                        }
                      -- TODO: viewport

              convertCursorPos (x, y) = (floor x, floor y)

              initInput w h = Input $ H.singleton Resize [
                        emptyEventData {
                                dataFramebufferSize = Just (w, h)
                        }]

              insertEvent e = H.insertWith (flip (++)) e . return

              modifyEvents m f = modifyMVar_ m $ return . f

              emptyEventData = EventData {
                                dataFramebufferSize = Nothing,
                                dataPointer = Nothing,
                                dataButton = Nothing,
                                dataKey = Nothing }

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
toKey Key'Unknown = KeyUnknown
