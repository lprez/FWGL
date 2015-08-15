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
                                   global LightView3 lightMat $
                                   object1Trans building )
                                $ \shadowMap ->
                                        [ layerPrg sceneProgram $
                                          global LightView3 lightMat $
                                          global LightPos3 lightPos $
                                          globalTexture ShadowMap shadowMap $
                                          objectVP cameraMat [ floor
                                                             , light
                                                             , building ]
                                        {- , elements [depthCube shadowMap] -} ]
        where floor = scaleV (Vec3 20 0.2 20) . cube . colorTex $
                                visible 240 230 180
              building = pos (Vec3 12 0.2 0) . scale 0.5 $
                                geom (colorTex white) buildingGeom
              depthCube map = pos (Vec3 0.7 0.7 0) . scale 0.2 $ cube map
              cameraView = fpsMovingCamera (Vec3 (- 3) 1.3 (- 10)) 0.3 >>^
                                \(pos, (pitch, yaw)) -> cameraMat4 pos pitch yaw
              cameraViewProj = cameraView >>^ \view -> (view .*.) .
                                        perspectiveMat4Size 100000 0.5 100 
              lightCube = lightPos >>^ \(Vec3 x y z) ->
                                pos (Vec3 (x - 0.2) y z) . scale 0.1 $
                                        cube (colorTex yellow)

lightPos :: SF a Vec3
lightPos = time >>^ \t -> let tmod = realToFrac $ mod' t 6000 / 2000
                              offset = if tmod < 1.5 then tmod else 3 - tmod
                          in Vec3 (- 6.1) 5 (0.75 - offset)

lightViewProj :: SF (Input ()) Mat4
lightViewProj = identity &&& (lightPos >>^ view)
                >>^ \(_, view) -> view .*. orthoMat4 20 0.53 (- 1) 1 (-1) 1
        where view pos = lookAtMat4 pos (Vec3 (- 100) 1 0) (Vec3 0 1 0)

fpsMovingCamera :: Vec3 -> Float -> SF (Input ()) (Vec3, (Float, Float))
fpsMovingCamera ipos sp = key KeyW &&& key KeyA &&& key KeyS &&& key KeyD &&&
                          pointer &&& size >>> flip sscan (ipos, (pi, 0)) update
        where update (Vec3 x y z, (pitch, yaw))
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
                        in (Vec3 (x + offX) y (z + offZ), (newPitch, newYaw))

sceneProgram :: Program Uniforms Attributes
sceneProgram = program vertexShader fragmentShader

depthProgram :: Program DepthUniforms DepthAttributes
depthProgram = program depthVertexShader depthFragmentShader

main :: IO ()
main = do Right o <- loadOBJ "building.obj"
          fwgl . run $ mainSF o
