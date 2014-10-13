module Main where

import Data.Fixed (mod')
import FWGL
import FRP.Yampa
import System.Random

data Box = Box V3 V3

outline :: Color -> Texture
outline c = mkTexture 128 128 [ if x < 5 || y < 5 || x > 123 || y > 123
                                        then Color 0 50 50 255
                                        else c
                              | x <- [ 0 .. 127 ], y <- [ 0 .. 127 ] ]

width :: Num a => a
width = 640

walls :: RandomGen g => g -> SF Input [Box]
walls r = noiseR (- 1.8, 1.8) r >>> sscan (step (0 :: Int)) []
        where step len (b : bs) newX = advance b $ step (len + 1) bs newX
              step len [] newX = if len < 40 && abs newX `mod'` 0.1 < 0.003
                                        then [ Box (V3 newX 1.8 (- 2))
                                                        (V3 0.2 0.1 0.1) ]
                                        else []
              advance (Box (V3 x y z) hs) =
                      if - y < 1.4 then (Box (V3 x (y - 0.02) z) hs :)
                                   else id

car :: SF Input Box
car = pointer >>^ \(x, _) -> Box (V3 (fromIntegral (x * 3) / width - 1.5)
                                     (- 0.8)
                                     (- 2))
                                 (V3 0.1 0.1 0.1)

death :: SF (Box, [Box]) Bool
death = arrPrim $ \(c, ws) -> any (collision c) ws

collision :: Box -> Box -> Bool
collision (Box (V3 x y z) (V3 hx hy hz)) (Box (V3 x' y' z') (V3 hx' hy' hz')) =
        not (abs (x - x') > (hx + hx') ||
             abs (y - y') > (hy + hy') ||
             abs (z - z') > (hz + hz'))

drawBox :: Texture -> Box -> Object
drawBox tex (Box p (V3 hx hy hz)) = texture tex .
                                    translate p .
                                    scaleV (V3 (hx) (hy) (hz)) $
                                    cube 1

draw :: SF (Box, [Box]) Scene
draw = arrPrim $ \(c, ws) -> perspective 1000 0.3 100 (4 / 3) $
                                drawBox outRed c : map (drawBox outWhite) ws
        where outRed = outline red
              outWhite = outline white

mainSig :: RandomGen g => g -> SF Input (Scene, Audio)
mainSig r = car &&& walls r >>>
            draw &&& death >>^
            (\(scene, ded) -> if ded then redScreen else scene) >>^
            (\scene -> (scene, Audio))
        where redScreen = scene [color red $ cube 1]

main :: IO ()
main = newStdGen >>= run "canvas" . mainSig
