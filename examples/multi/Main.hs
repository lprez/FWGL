{-# LANGUAGE CPP #-}

module Main where

import Control.Concurrent
import FWGL
import FWGL.Graphics.D2

#ifdef __GHCJS__
import FWGL.Backend.JavaScript
#else
import FWGL.Backend.GLFW.GL20
#endif

mainSF :: Color -> SF (Input ()) Output
mainSF col = pointer &&& size &&& time &&& repeatedly 100 () >>^
                \((x, y), ((w, h), (time, trigger))) ->
                        let (w', h') = (resize x w, resize y h)
                        in drawEff [layerS . viewScreen idmtx $
                                        [ trans (Vec2 (fromIntegral w / 2)
                                                (fromIntegral h / 2))
                                        . scaleV (Vec2 (fromIntegral w - 40)
                                                       (fromIntegral h - 40))
                                        . rect $ colorTex col]
                                   ] $
                           if isEvent trigger && (w /= w' || h /= h')
                           then setSize w' h' >>
                                setTitle (show w' ++ ", " ++ show h')
                           else return ()
        where resize pos max =
                if pos < 20 && pos > 0 && max > 40
                then max - pos
                else if pos > max - 20 && max < 1024
                then max * 2 - pos
                else max

main :: IO ()
main = newEmptyMVar >>= \end -> fwgl $
        do mapIO fork $ do runTo "#canvas2" (return ()) (mainSF blue)
                           liftIO $ putMVar end ()
           runTo "#canvas1" (return ()) $ mainSF red
           liftIO $ takeMVar end
        where fork =
#ifdef __GHCJS__
                     forkIO
#else
                     forkOS
#endif
