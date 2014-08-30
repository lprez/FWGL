{-# LANGUAGE Arrows #-}

module Main where

import FRP.Yampa
import FWGL

mainSF :: SF Input (Scene, Audio)
mainSF = proc inp -> do (x, y) <- pointer -< inp

                        mLeft <- mouse MouseLeft -< inp
                        leftCount <- accumHoldBy (\c _ -> c + 0.05) 1 -< mLeft

                        mRight <- mouse MouseRight -< inp
                        rightCount <- accumHoldBy (\c _ -> c + 0.05) 0 -< mRight

                        let scaleFact = leftCount - rightCount
                            transformedCube = rotX (fromIntegral y / 120) $
                                              rotY (fromIntegral x / 160) $
                                              scale scaleFact $
                                              cube 0.5

                        returnA -< ([transformedCube], Audio)

main :: IO ()
main = run "canvas" mainSF
