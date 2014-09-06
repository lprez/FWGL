{-# LANGUAGE Arrows #-}

module Main where

import MonkeyOBJ

import Data.Fixed (mod')
import Data.List (unfoldr)
import FRP.Yampa
import FWGL

mouseCount :: Num a => a -> MouseButton -> SF Input a
mouseCount i mb = mouse mb >>> accumHoldBy (\c _ -> c + 1) i

monkeyTex :: String
monkeyTex = "monkeyTex.png"

gradient :: Color -> Color -> Texture
gradient c1 c2 = mkTexture (floor w) (floor w) $ unfoldr f (0, 0)
        where cerp (Color r g b a) (Color r' g' b' a') t =
                Color (lerp r r' t) (lerp g g' t) (lerp b b' t) (lerp a a' t)
              lerp v v' t = floor $ (1 - t) * (fromIntegral v)
                                  + t * (fromIntegral v')
              swing x mx = if x >= mx / 2 then mx - x else x
              f (x, y) | x == w = f (0 :: Float, y + 1)
                       | y == w = Nothing
                       | otherwise = Just $ ( cerp c1 c2 . (/ (w / 2)) $
                                                swing x w + swing y w
                                            , (x + 1, y) )
              w = 256 :: Num a => a

mainSF :: SF Input (Scene, Audio)
mainSF = proc inp -> do (x, y) <- pointer -< inp

                        tm <- time >>> arr realToFrac -< inp
                        leftCount <- mouseCount 4 MouseLeft -< inp
                        rightCount <- mouseCount 0 MouseRight -< inp

                        let scaleFact = (leftCount - rightCount) / 20
                            transformedMonkey = texture (textureURL monkeyTex) $
                                                rotX (fromIntegral y / 120 - 1) $
                                                rotY (fromIntegral x / 160 + 1) $
                                                scale scaleFact $
                                                geom monkeyOBJ
                            transformedCube o = texture (gradient yellow red) $
                                                rotX (mod' ((tm + o) / 200) $
                                                           (pi * 2)) $
                                                rotY (mod' ((tm + o) / 400) $
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
