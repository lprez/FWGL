{-# LANGUAGE Arrows, CPP #-}

module Main where

import Data.Fixed (mod')
import Data.IORef
import FWGL
import FWGL.Graphics.D3

#ifdef __GHCJS__
import FWGL.Backend.JavaScript
#else
import FWGL.Backend.GLFW.GL20
#endif

import Shaders

newtype CObjectId = CObjectId Vec3 deriving Eq

viewMat :: SF (Input a) Mat4
viewMat = size >>^ \(w, h) ->
        lookAtMat4 (Vec3 0.93 0.74 0.35) (Vec3 0 0.2 0) (Vec3 0 1 0)
        .*. perspectiveMat4 0.4 1000 100 (fromIntegral w / fromIntegral h)

cubes :: [SF (Event CObjectId) (Object3D, CObjectId)]
cubes = map pcube $ zip [1 ..] [ Vec3 x y (z + 0.185)
                               | x <- coords, y <- coords, z <- coords ]
        where pcube :: (Float, Vec3)
                    -> SF (Event CObjectId) (Object3D, CObjectId)
              pcube (i, p) = let oid = CObjectId $ Vec3 i 0 0 in
                arr (fmap $ \oid' -> if oid == oid' then activeTexture
                                                    else inactiveTexture)
                >>> hold inactiveTexture
                >>^ flip (,) oid . trans p . scale 0.045 . cube

              inactiveTexture = cubeTexture 0
              activeTexture = cubeTexture 255

              coords = [ - 0.37, - 0.185 .. 0.37 ]

scene :: SF (Input (Event CObjectId)) Output
scene = viewMat &&& custom >>> sceneLayer >>^ draw . (: [])

sceneLayer :: SF (Mat4, Event CObjectId) Layer
sceneLayer = second (parB cubes) &&& time >>^
                \((viewMat, cubes), t) ->
                     layerS . view viewMat $ grid (realToFrac t) : map fst cubes
        where grid t = trans (Vec3 0 0 0) . scale (2 + mod' t 100000 / 100000)
                       $ cube gridTexture

pick :: (Event CObjectId -> Effect ()) -> SF (Input a) Output
pick putOid = pointer &&& (viewMat &&& identity >>> pickLayer) &&& size
              >>^ \((px, py), (layer, (w, h))) ->
        let (rw, rh) = (ceilp2 w, ceilp2 h)
        in fastStep . (>>= putOid) . liftDraw $
                do ([Color r g b _], ts) <- renderLayer $
                                renderColorInspect rw rh layer
                                                   (div (px * rw) w)
                                                   (rh - div (py * rh) h)
                                                   1 1 $ flip const
                   mapM_ removeTexture ts
                   return . Event . CObjectId $
                                Vec3 (fromIntegral r) (fromIntegral g)
                                     (fromIntegral b)
        where ceilp2 x = 2 ^ (ceiling $ log (fromIntegral x) / log 2)

pickLayer :: SF (Mat4, Input a) Layer
pickLayer = arr fst &&& (constant NoEvent >>> parB cubes)
                >>^ \(viewMat, cubes) ->
                      layer pickProgram .
                      view viewMat $
                      map (\(e, CObjectId oid) -> ObjectId -= oid :~> e)
                          cubes
        where pickProgram :: Program PickUniforms PickAttributes
              pickProgram = program pickVertexShader pickFragmentShader


mainSF :: (Event CObjectId -> Effect ()) -> SF (Input (Event CObjectId)) Output
mainSF putOid = (mouseMove >>> sscan limit NoEvent) &&& scene &&& pick putOid >>^
                \(ev, (sceneOut, pickOut)) -> event sceneOut (const pickOut) ev
        where limit NoEvent (Event x) = Event x
              limit _ _ = NoEvent

main :: IO ()
main = newIORef NoEvent >>= \oidRef ->
       fwgl $ run' (atomicModifyIORef oidRef $ (,) noEvent)
                   (mainSF $ liftIO . writeIORef oidRef)

gridTexture :: Texture
gridTexture = mkTexture 512 512
                        [ let fact = 1 - (signum $ mod (fromIntegral x) 32
                                                 * mod (fromIntegral y) 32)
                          in Color 64 255 64 (255 * fact)
                        |  x <- [ 0 .. 511 ], y <- [ 0 .. 511 ] ]

cubeTexture :: Integral i => i -> Texture
cubeTexture i = mkTexture 64 64 [ Color (255 - 2 * (fromIntegral $ abs x + abs y))
                                        (fromIntegral i) 0 255
                                | x <- [ - 31, -30 .. 32 ], y <- [ - 31, -30 .. 32] ]
