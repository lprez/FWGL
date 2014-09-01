{-# LANGUAGE Arrows #-}

module Main where

import Monkey

import FRP.Yampa
import FWGL

mainSF :: SF Input (Scene, Audio)
mainSF = proc inp -> do (x, y) <- pointer -< inp

                        --

                        mLeft <- mouse MouseLeft -< inp
                        leftCount <- accumHoldBy (\c _ -> c + 1) 1 -< mLeft

                        mRight <- mouse MouseRight -< inp
                        rightCount <- accumHoldBy (\c _ -> c + 1) 0 -< mRight

                        let scaleFact = (leftCount - rightCount) / 20
                            transformedMonkey = rotX (fromIntegral y / 120) $
                                                rotY (fromIntegral x / 160) $
                                                scale scaleFact $
                                                geom monkey
                            transformedCube = translate (V3 (scaleFact * 10) 0 0) $
                                              cube 0.3
                                               

                        returnA -< ([ transformedCube
                                    , transformedMonkey ], Audio)

main :: IO ()
main = run "canvas" mainSF
