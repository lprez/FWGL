{-# LANGUAGE Arrows #-}

module Main where

import Monkey

import Data.Fixed (mod')
import FRP.Yampa
import FWGL

mainSF :: SF Input (Scene, Audio)
mainSF = proc inp -> do (x, y) <- pointer -< inp

                        tm <- time >>> arr realToFrac -< inp

                        mLeft <- mouse MouseLeft -< inp
                        leftCount <- accumHoldBy (\c _ -> c + 1) 1 -< mLeft

                        mRight <- mouse MouseRight -< inp
                        rightCount <- accumHoldBy (\c _ -> c + 1) 0 -< mRight

                        let scaleFact = (leftCount - rightCount) / 3
                            transformedMonkey = rotX (fromIntegral y / 120 - 1) $
                                                rotY (fromIntegral x / 160 + 1) $
                                                scale scaleFact $
                                                geom monkey
                            transformedCube o = rotX (mod' ((tm + o) / 100) $
                                                           (pi * 2)) $
                                                rotY (mod' ((tm + o) / 200) $
                                                           (pi * 2)) $
                                                translate (V3 9 0 0) $
                                                rotZ (mod' ((tm + o) / 400)
                                                           (pi * 2)) $
                                                cube 0.1
                                               

                        returnA -< ([ transformedCube 0
                                    , transformedCube 628
                                    , transformedCube 1256
                                    , transformedCube 1884
                                    , transformedMonkey ], Audio)

main :: IO ()
main = run "canvas" mainSF
