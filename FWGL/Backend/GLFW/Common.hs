module FWGL.Backend.GLFW.Common (
        loadImage,
        setup,
        ClientAPI(..)
) where

import Codec.Picture
import Codec.Picture.Types (promoteImage)
import qualified Data.HashMap.Strict as H
import Data.IORef
import Data.Vector.Storable (unsafeWith)
import Foreign.Ptr
import FRP.Yampa
import FWGL.Backend.GLES hiding (Image)
import FWGL.Input
import Graphics.UI.GLFW as GLFW

loadImage :: Num sz => String -> ((sz, sz, Ptr ()) -> IO a) -> IO a
loadImage path c = do eimg <- readImage path
                      case eimg of
                            Left err -> error err
                            Right img ->
                                    case convert img of
                                        Image w h v -> unsafeWith v $
                                                \p -> c (fromIntegral w,
                                                         fromIntegral h,
                                                         castPtr p)
        where convert :: DynamicImage -> Image PixelRGB8
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
           drawStateRef <- initState w h () >>= newIORef
           reactStateRef <- reactInit (return . Input $ H.empty)
                                      (\_ _ -> actuate win drawStateRef)
                                      sigf
           setTime 0
           setWindowRefreshCallback win $ Just . const $ refresh reactStateRef
           let loop = do waitEvents
                         close <- windowShouldClose win
                         if close
                                then do destroyWindow win
                                        terminate
                                else loop
           loop
        where refresh rsf = do (Just tm) <- getTime
                               react rsf (tm, Just $ Input H.empty)
                               setTime 0
              actuate win stateRef out = do newState <- readIORef stateRef
                                                        >>= draw out ()
                                            swapBuffers win
                                            writeIORef stateRef newState
                                            return False
