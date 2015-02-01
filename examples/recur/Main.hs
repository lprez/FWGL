{-# LANGUAGE Arrows, CPP #-}

module Main where

import FWGL
import FWGL.Graphics.D2
import FRP.Yampa

#ifdef __GHCJS__
import FWGL.Backend.JavaScript
#else
import FWGL.Backend.GLFW.GL20
#endif

width :: Num a => a
width = 640

quad :: SF (Float, Input) Float
quad = proc (mx, inp) -> do
        d <- key KeyD >>> arrPrim (tagWith 0.02) -< inp
        a <- key KeyA >>> arrPrim (tagWith $ -0.02) -< inp
        dAccumHoldBy (\ x (dx, mx) -> if x + dx + 0.4 < mx
                                                then x + dx
                                                else x) 0
                -< joinE (mergeBy (+) d a) (Event mx)

wall :: SF (Float, Input) Float
wall = proc (mx, inp) -> do
        (px, _) <- pointer -< inp
        sscan (\ x (mx, px) -> if px - 0.4 > mx then px else x) 1
                -< (mx, ((fromIntegral px / width) * 2) - 1)

mainSF :: SF Input Output
mainSF = proc inp -> do
        rec qx <- quad -< (wx, inp)
            wx <- wall -< (qx, inp)
        let cubeObj = pos (V2 qx 0) $
                      rect (V2 0.4 0.4) (colorTex red)
            wallObj = pos (V2 wx 0) $
                      rect (V2 0.4 2) (colorTex blue)
        returnA -< Output [elements [cubeObj, wallObj]] Audio

main :: IO ()
main = run mainSF
