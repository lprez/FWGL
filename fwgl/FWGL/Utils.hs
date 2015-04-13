module FWGL.Utils (
        screenScale,
        perspective4,
        perspectiveView
) where

import FRP.Yampa
import FWGL.Input
import FWGL.Graphics.Custom
import FWGL.Vector

-- | Generate a view matrix that transforms the pixel coordinates in OpenGL
-- coordinates.
screenScale :: SF (Input a) M3
screenScale = size >>^ \(x, y) -> scaleMat3 (V2 (1 / fromIntegral x)
                                                (1 / fromIntegral y))

-- | Generate a perspective view matrix using the aspect ratio of the
-- framebuffer.
perspective4 :: Float   -- ^ Far
             -> Float   -- ^ Near
             -> Float   -- ^ FOV
             -> SF (Input a) M4
perspective4 f n fov =
        size >>^ \(w, h) -> perspectiveMat4 f n fov
                                            (fromIntegral w / fromIntegral h)

-- | Combine a perspective and a view matrix.
perspectiveView :: Float    -- ^ Far
                -> Float    -- ^ Near
                -> Float    -- ^ FOV
                -> SF (Input a, M4) M4
perspectiveView far near fov  =
        perspective4 far near fov *** identity
        >>^ \(perspMat, viewMat) -> mul4 viewMat perspMat

-- | Like 'dynamic', but instead of comparing the two objects it checks the
-- event with the new object.
dynamicE :: Object g i -- ^ Initial 'Object'.
         -> SF (Event (Object g i)) (Object g i)
dynamicE = dynamicG $ flip const

-- | Automatically deallocate the previous mesh from the GPU when it changes.
dynamic :: SF (Object g i) (Object g i)
dynamic = undefined
{-
dynamic =
        dynamicG (\ o n -> if objectGeometry o == objectGeometry n
                                then Event n
                                else NoEvent
        ) nothing
-}

dynamicG :: (Object g i -> a -> Event (Object g i)) -> (Object g i) -> SF a (Object g i)
dynamicG = undefined
{-
dynamicG f i = flip sscan i $ \ oldObj inp ->
                case f oldObj inp of
                        Event (SolidObject (Solid (StaticGeom newG) mat tex)) ->
                                SolidObject (
                                        Solid (DynamicGeom (objectGeometry oldObj)
                                                           newG)
                                              mat
                                              tex
                                )
                        NoEvent -> case oldObj of
                                        SolidObject (
                                                Solid (DynamicGeom _ new) mat t
                                                ) -> SolidObject (
                                                        Solid (StaticGeom new)
                                                              mat
                                                              t
                                                        )
                                        _ -> oldObj
                        _ -> error "dynamicG: not a Geometry."
-}

