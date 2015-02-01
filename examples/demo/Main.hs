{-# LANGUAGE Arrows, CPP #-}

module Main where

import MonkeyOBJ

import Data.Fixed (mod')
import Data.List (unfoldr)
import FRP.Yampa
import FWGL
import FWGL.Graphics.D3

#ifdef __GHCJS__
import FWGL.Backend.JavaScript
#else
import FWGL.Backend.GLFW.GL20
#endif

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

mainSF :: SF Input Output
mainSF = proc inp -> do (x, y) <- pointer -< inp

                        tm <- time >>> arr realToFrac -< inp
                        leftCount <- mouseCount 4 MouseLeft -< inp
                        rightCount <- mouseCount 0 MouseRight -< inp

                        let scaleFact = (leftCount - rightCount) / 100
                            timeAng o f = mod' (tm / f + o) $ pi * 2
                            transformedMonkey =
                                    pos (V3 0 0 (- 1)) $
                                    rotY (fromIntegral x / 160 - 1) $
                                    rotX (fromIntegral y / 120 - 1) $
                                    scale scaleFact $
                                    geom (textureURL monkeyTex) monkeyOBJ
                            transformedCube o tex =
                                    pos (
                                            V3 (sin (timeAng o 800) / 6)
                                               (sin (timeAng o 800) / 6)
                                               (cos (timeAng o 800) / 5 - 1)) $
                                    rotX (timeAng o 200) $
                                    rotY (timeAng o 400) $
                                    scale 0.02 $
                                    cube tex
                                               

                        viewMatrix <- perspective4 10000 0.12 100 -< inp
                        returnA -< flip Output Audio . return . view viewMatrix
                                        $ [ transformedCube 0 gradRedYellow
                                          , transformedCube (pi / 2) gradGreenBlue
                                          , transformedCube pi gradRedYellow
                                          , transformedCube (pi * 3 / 2) gradGreenBlue
                                          , transformedMonkey ]
        where gradRedYellow = gradient red yellow
              gradGreenBlue = gradient green blue

main :: IO ()
main = run mainSF
