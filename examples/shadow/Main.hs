{-# LANGUAGE CPP #-}

module Main where

import Data.Fixed (mod')

import FWGL
import FWGL.Graphics.D3
#ifdef __GHCJS__
import FWGL.Backend.JavaScript
#else
import FWGL.Backend.GLFW.GL20
#endif

import Shaders

mainSF :: Geometry Geometry3 -> SF (Input ()) Output
mainSF buildingGeom = 
        cameraViewProj &&& lightViewProj &&&
        lightPos &&& lightCube &&& time >>^
                \(cameraMat, (lightMat, (lightPos, (light, time)))) ->
                    draw . (: []) $
                        depthSubLayer 1024 1024
                                 ( layerPrg depthProgram $ 
                                   global (undefined :: LightView3) lightMat $
                                   object1Trans building )
                                $ \shadowMap ->
                                        [ layerPrg sceneProgram $
                                          global (undefined :: LightView3)
                                                 lightMat $
                                          global (undefined :: LightPos3)
                                                 lightPos $
                                          globalTexture (undefined :: ShadowMap)
                                                        shadowMap $
                                          object cameraMat [ floor
                                                           , light
                                                           , building ]
                                        {- , elements [depthCube shadowMap] -} ]
        where floor = scaleV (V3 20 0.2 20) . cube . colorTex $
                                                        visible 240 230 180
              building = pos (V3 12 0.2 0) . scale 0.5 $
                                geom (colorTex white) buildingGeom
              depthCube map = pos (V3 0.7 0.7 0) . scale 0.2 $ cube map
              cameraView = fpsMovingCamera (V3 (- 3) 1.3 (- 10)) 0.3 >>^
                                \(pos, (pitch, yaw)) -> cameraMat4 pos pitch yaw
              cameraViewProj = identity &&& cameraView >>>
                               perspectiveView 100000 0.5 100
              lightCube = lightPos >>^ \(V3 x y z) ->
                                pos (V3 (x - 0.2) y z) . scale 0.1 $
                                        cube (colorTex yellow)

lightPos :: SF a V3
lightPos = time >>^ \t -> let tmod = realToFrac $ mod' t 3000 / 500
                              offset = if tmod < 3 then tmod else 6 - tmod
                          in V3 (- 3.1) 5 (1.5 - offset)

lightViewProj :: SF (Input ()) M4
lightViewProj = identity &&& (lightPos >>^ view)
                >>> perspectiveView 10 0.53 90
        where view pos = lookAtMat4 pos (V3 100 1 0) (V3 0 (- 1) 0)

fpsMovingCamera :: V3 -> Float -> SF (Input ()) (V3, (Float, Float))
fpsMovingCamera ipos sp = key KeyW &&& key KeyA &&& key KeyS &&& key KeyD &&&
                          pointer &&& size >>> flip sscan (ipos, (pi, 0)) update
        where update (V3 x y z, (pitch, yaw))
                     (kw, (ka, (ks, (kd, ((ptrX, ptrY), ((w, h))))))) =
                        let par = (if isEvent kw then - sp else 0) +
                                  (if isEvent ks then sp else 0)
                            perp = (if isEvent kd then sp else 0) +
                                   (if isEvent ka then - sp else 0)
                            offX = sin yaw * par + cos yaw * perp
                            offZ = cos yaw * par - sin yaw * perp
                            fi = fromIntegral :: Int -> Float
                            newYaw = - 2 * pi * fi ptrX / fi w
                            newPitch = - 2 * pi * fi ptrY / fi h + pi
                        in (V3 (x + offX) y (z + offZ), (newPitch, newYaw))

sceneProgram :: Program Uniforms Attributes
sceneProgram = program vertexShader fragmentShader

depthProgram :: Program DepthUniforms DepthAttributes
depthProgram = program depthVertexShader depthFragmentShader

main :: IO ()
main = do Right o <- loadOBJ "building.obj"
          run $ mainSF o
