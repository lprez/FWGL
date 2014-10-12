{-# LANGUAGE Arrows #-}

module Main where

import FWGL
import FRP.Yampa

width :: Num a => a
width = 640

rect :: SF (Float, Input) Float
rect = proc (mx, inp) -> do
        d <- key 'D' >>> arrPrim (tagWith 0.02) -< inp
        a <- key 'A' >>> arrPrim (tagWith $ -0.02) -< inp
        dAccumHoldBy (\ x (dx, mx) -> if x + dx + 0.4 < mx
                                                then x + dx
                                                else x) 0
                -< joinE (mergeBy (+) d a) (Event mx)

wall :: SF (Float, Input) Float
wall = proc (mx, inp) -> do
        (px, _) <- pointer -< inp
        sscan (\ x (mx, px) -> if px - 0.4 > mx then px else x) 1
                -< (mx, ((fromIntegral px / width) * 2) - 1)

mainSF :: SF Input (Scene, Audio)
mainSF = proc inp -> do
        rec rct <- rect -< (wal, inp)
            wal <- wall -< (rct, inp)
        let cubeObj = color red $ translate (V3 rct 0 0)
                                $ cube 0.2
            wallObj = color blue $ translate (V3 wal 0 0)
                                 $ scaleV (V3 1 5 1) $ cube 0.2
        returnA -< (scene [cubeObj, wallObj], Audio)

main :: IO ()
main = run "canvas" mainSF
