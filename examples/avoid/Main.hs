module Main where

import Data.Fixed (mod')
import FWGL
import FWGL.Backend.JavaScript
import FWGL.Graphics.D3
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
              step len [] newX = [ Box (V3 newX 1.8 (- 2)) (V3 0.2 0.1 0.1)
                                 | len < 40 && abs newX `mod'` 0.1 < 0.003 ]
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

drawBox :: Texture -> Box -> Element
drawBox tex (Box p (V3 hx hy hz)) = pos p .
                                    scaleV (V3 hx hy hz) $
                                    cube tex

draw :: SF (Box, [Box]) [Element]
draw = arrPrim $ \(c, ws) -> drawBox outRed c : map (drawBox outWhite) ws
        where outRed = outline red
              outWhite = outline white

mainSig :: RandomGen g => g -> SF Input Output
mainSig r = perspective4 1000 0.3 100 &&& car &&& walls r >>>
            second (draw &&& death) >>^
            second (\(els, ded) -> if ded then redScreen else els) >>^
            (\(viewMat, els) -> Output [view viewMat els] Audio)
        where redScreen = [cube $ colorTex red]

main :: IO ()
main = newStdGen >>= run . mainSig
