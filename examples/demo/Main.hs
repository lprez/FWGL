{-# LANGUAGE Arrows, CPP #-}

module Main where

import Data.IORef
import Data.Fixed (mod')
import Data.List (unfoldr)
import FRP.Yampa
import FWGL
import FWGL.Graphics.D3

#ifdef __GHCJS__
import FWGL.Backend.JavaScript
import GHCJS.Foreign
import GHCJS.Types
#else
import FWGL.Backend.GLFW.GL20
#endif

mainSF :: SF (Input (Maybe (Geometry Geometry3D))) Output
mainSF = proc inp -> do tm <- time >>> arr realToFrac -< inp
                        monkey <- monkey -< inp
                        cubes <- parB [ rotatingCube 0 gradRedYellow
                                      , rotatingCube (pi / 2) gradGreenBlue
                                      , rotatingCube pi gradRedYellow
                                      , rotatingCube (pi * 3 / 2) gradGreenBlue ]
                              -< inp
 
                        returnA -< draw [ layerS
                                                . viewPersp 0.12 10000 100 idmtx
                                                $ monkey : cubes ]

        where gradRedYellow = gradient red yellow
              gradGreenBlue = gradient green blue

rotatingCube :: Float -> Texture -> SF (Input a) Object3D
rotatingCube angleOff tex =
        proc inp -> do tm <- time >>> arr realToFrac -< inp

                       let angle factor = mod' (tm / factor + angleOff) $ pi * 2

                       returnA -< ( trans (
                                            Vec3 (sin (angle 800) / 6)
                                                 (sin (angle 800) / 6)
                                                 (cos (angle 800) / 5 - 1)) $
                                    rotX (angle 200) $
                                    rotY (angle 400) $
                                    scale 0.02 $
                                    cube tex )

monkey :: SF (Input (Maybe (Geometry Geometry3D))) Object3D
monkey = proc inp -> do (x, y) <- pointer -< inp
                        (width, height) <- size -< inp
                        scaleFact <- mouseScale -< inp
                        maybeGeometry <- custom -< inp

                        let geometry = case maybeGeometry of
                                            Just g -> g
                                            Nothing -> emptyGeometry

                        returnA -< ( trans (Vec3 0 0 (- 1)) $
                                     rotY (2 - fromIntegral x * 4 /
                                           fromIntegral width) $
                                     rotX (2 - fromIntegral y * 4
                                           / fromIntegral height) $
                                     scale scaleFact $
                                     mesh (textureURL monkeyTex) geometry )
        where emptyGeometry = mkGeometry3D [] [] [] []

mouseScale :: SF (Input a) Float
mouseScale = proc inp -> do leftCount <- mouseCount 4 MouseLeft -< inp
                            rightCount <- mouseCount 0 MouseRight -< inp

                            returnA -< (leftCount - rightCount) / 100
        where mouseCount i mb = mouse mb >>> accumHoldBy (\c _ -> c + 1) i

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

main :: IO ()
main = do monkeyOBJ <- newIORef Nothing
#ifdef __GHCJS__
          loadDiv $ toJSString "<b>Loading monkey.obj...</b>"
#endif
          loadOBJAsync "monkey.obj" $ \egeom ->
                  case egeom of
                       Left err -> putStrLn err
                       Right geom -> do writeIORef monkeyOBJ . Just $! geom
#ifdef __GHCJS__
                                        loadDiv $ toJSString ""
#endif
          backend $ run' (readIORef monkeyOBJ) mainSF

#ifdef __GHCJS__
foreign import javascript unsafe "document.getElementById('load').innerHTML = $1;"
        loadDiv :: JSString -> IO ()
#endif
