{-# LANGUAGE CPP #-}

module Main where

import FWGL
import FWGL.Graphics.D2
import FRP.Yampa

#ifdef __GHCJS__
import FWGL.Backend.JavaScript
#else
import FWGL.Backend.GLFW.GL20
#endif

import Program

surface :: Geometry Geometry2
surface = mkGeometry2 (quads (- 0.5) (- 0.5))
                      (quads 0 0)
                      (concat [ let i' = i * 4
                                in [ i', i' + 1, i' + 2, i', i' + 3, i' + 2 ]
                              | i <- [ 0 .. precision ^ 2 - 1 ]])
        where quads ix iy = concat [ quad ix iy x y (1 / precision)
                                   | x <- [ 0 .. precision - 1 ]
                                   , y <- [ 0 .. precision - 1 ]]
              quad ix iy qx qy w =
                let x = ix + qx * w
                    y = iy + qy * w
                in [ V2 x y, V2 (x + w) y, V2 (x + w) (y + w), V2 x (y + w) ]

              precision :: Num a => a
              precision = 20

main :: IO ()
main = run $ pointer >>^ \(x, y) -> Output [
                layerPrg distProgram $
                global (undefined :: Pointer)
                       (V2 (fromIntegral x / 640 - 0.5)
                           (- fromIntegral y / 480 + 0.5))
                       obj
       ] Audio
        where tex = textureFile "tex.png"
              obj = object idMat3 [geom tex surface]

distProgram :: Program Uniforms Attributes
distProgram = program vertexShader fragmentShader
