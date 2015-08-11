module FWGL.Utils (
        screenScale,
        perspective4,
        perspectiveView
) where

import Data.Vect.Float
import FRP.Yampa
import FWGL.Input
import FWGL.Graphics.Custom
import FWGL.Transformation

-- | Generate a view matrix that transforms the pixel coordinates in OpenGL
-- coordinates.
screenScale :: SF (Input a) Mat3
screenScale = size >>^ \(x, y) -> scaleMat3 (Vec2 (2 / fromIntegral x)
                                                  (2 / fromIntegral y))

-- | Generate a perspective view matrix using the aspect ratio of the
-- framebuffer.
perspective4 :: Float   -- ^ Far
             -> Float   -- ^ Near
             -> Float   -- ^ FOV
             -> SF (Input a) Mat4
perspective4 f n fov =
        size >>^ \(w, h) -> perspectiveMat4 f n fov
                                            (fromIntegral w / fromIntegral h)

-- | Combine a perspective and a view matrix.
perspectiveView :: Float    -- ^ Far
                -> Float    -- ^ Near
                -> Float    -- ^ FOV
                -> SF (Input a, Mat4) Mat4
perspectiveView far near fov  =
        perspective4 far near fov *** identity
        >>^ \(perspMat, viewMat) -> perspMat .*. viewMat
