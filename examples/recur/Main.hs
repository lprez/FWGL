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

quad :: SF (Float, Input ()) Float
quad = proc (mx, inp) -> do
        d <- key KeyD >>> arrPrim (tagWith 0.02) -< inp
        a <- key KeyA >>> arrPrim (tagWith $ -0.02) -< inp
        dAccumHoldBy (\ x (dx, mx) -> if x + dx + 0.4 < mx
                                                then x + dx
                                                else x) 0
                -< joinE (mergeBy (+) d a) (Event mx)

wall :: SF (Float, Input ()) Float
wall = proc (mx, inp) -> do
        (px, _) <- pointer -< inp
        (width, _) <- size -< inp
        sscan (\ x (mx, px) -> if px - 0.4 > mx then px else mx + 0.4) 1
                -< (mx, ((fromIntegral px / fromIntegral width) * 2) - 1)

mainSF :: SF (Input ()) Output
mainSF = proc inp -> do
        rec qx <- quad -< (wx, inp)
            wx <- wall -< (qx, inp)
        let cubeObj = pos (Vec2 qx 0) $
                      rect (Vec2 0.4 0.4) (colorTex red)
            wallObj = pos (Vec2 wx 0) $
                      rect (Vec2 0.4 2) (colorTex blue)
        returnA -< draw [elements [cubeObj, wallObj]]

main :: IO ()
main = initialize >> run mainSF >> terminate
