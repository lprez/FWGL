module FWGL.Vector (
        V2(..),
        V3(..),
        V4(..),
        M2(..),
        M3(..),
        M4(..),
        vec2,
        vec3,
        vec4,
        dot4,
        mat2,
        mat3,
        mul3,
        mat4,
        mul4,
        transpose4,
        idMat4,
        transMat4,
        rotXMat4,
        rotYMat4,
        rotZMat4,
        rotAAMat4,
        scaleMat4,
        perspectiveMat4,
        cameraMat4,
        -- lookAtMat4,
        idMat3,
        transMat3,
        rotMat3,
        scaleMat3
) where

import Control.Applicative
import Data.Hashable
import Foreign.Storable
import Foreign.Ptr (castPtr)

-- | Two-dimensional vector.
data V2 = V2 !Float !Float deriving (Show, Eq)

-- | Three-dimensional vector.
data V3 = V3 !Float !Float !Float deriving (Show, Eq)

-- | Four-dimensional vector.
data V4 = V4 !Float !Float !Float !Float deriving (Show, Eq)

-- | 2x2 matrix.
data M2 = M2 !V2 !V2 deriving (Show, Eq)

-- | 3x3 matrix.
data M3 = M3 !V3 !V3 !V3 deriving (Show, Eq)

-- | 4x4 matrix.
data M4 = M4 !V4 !V4 !V4 !V4 deriving (Show, Eq)

instance Hashable V2 where
        hashWithSalt s (V2 x y) = hashWithSalt s (x, y)

instance Hashable V3 where
        hashWithSalt s (V3 x y z) = hashWithSalt s (x, y, z)

instance Hashable V4 where
        hashWithSalt s (V4 x y z w) = hashWithSalt s (x, y, z, w)

instance Storable V2 where
        sizeOf _ = 2 * sizeOf (undefined :: Float)
        alignment _ = alignment (undefined :: Float)
        peek ptr = V2 <$> peek (castPtr ptr)
                      <*> peekElemOff (castPtr ptr) 1
        poke ptr (V2 x y) = poke (castPtr ptr) x >> pokeElemOff (castPtr ptr) 1 y

instance Storable V3 where
        sizeOf _ = 3 * sizeOf (undefined :: Float)
        alignment _ = alignment (undefined :: Float)
        peek ptr = V3 <$> peek (castPtr ptr)
                      <*> peekElemOff (castPtr ptr) 1
                      <*> peekElemOff (castPtr ptr) 2
        poke ptr (V3 x y z) = zipWithM_ (pokeElemOff $ castPtr ptr)
                                        [0 .. 2] [x, y, z]
instance Storable V4 where
        sizeOf _ = 4 * sizeOf (undefined :: Float)
        alignment _ = alignment (undefined :: Float)
        peek ptr = V4 <$> peek (castPtr ptr) 
                      <*> peekElemOff (castPtr ptr) 1
                      <*> peekElemOff (castPtr ptr) 2
                      <*> peekElemOff (castPtr ptr) 3
        poke ptr (V4 x y z w) = zipWithM_ (pokeElemOff $ castPtr ptr)
                                          [0 .. 3] [x, y, z, w]

-- | Create a two-dimensional vector.
vec2 :: (Float, Float) -> V2
vec2 = uncurry V2

-- | Create a three-dimensional vector.
vec3 :: (Float, Float, Float) -> V3
vec3 (x, y, z) = V3 x y z

-- | Create a four-dimensional vector.
vec4 :: (Float, Float, Float, Float) -> V4
vec4 (x, y, z, w) = V4 x y z w

-- | 3D vector dot product.
dot3 :: V3 -> V3 -> Float
dot3 (V3 x y z) (V3 x' y' z') = x * x' + y * y' + z * z'

-- | 4D vector dot product.
dot4 :: V4 -> V4 -> Float
dot4 (V4 x y z w) (V4 x' y' z' w') = x * x' + y * y' + z * z' + w * w'

-- | Create a 2x2 matrix.
mat2 :: ( Float, Float
        , Float, Float ) -> M2
mat2 (a, a', b, b') = M2 (V2 a a') (V2 b b')

-- | Create a 3x3 matrix.
mat3 :: ( Float, Float, Float
        , Float, Float, Float
        , Float, Float, Float ) -> M3
mat3 (a1, a2, a3, b1, b2, b3, c1, c2, c3) =
        M3 (V3 a1 a2 a3)
           (V3 b1 b2 b3)
           (V3 c1 c2 c3)

mat4from3 :: ( Float, Float, Float
             , Float, Float, Float
             , Float, Float, Float ) -> M4
mat4from3 (a1, a2, a3, b1, b2, b3, c1, c2, c3) =
        M4 (V4 a1 a2 a3 0)
           (V4 b1 b2 b3 0)
           (V4 c1 c2 c3 0)
           (V4 0  0  0  1)

mat3from2 :: (Float, Float, Float, Float) -> M3
mat3from2 (a1, a2, b1, b2) = M3 (V3 a1 a2 0)
                                (V3 b1 b2 0)
                                (V3 0  0  1)

-- | Create a 4x4 matrix.
mat4 :: ( Float, Float, Float, Float
        , Float, Float, Float, Float
        , Float, Float, Float, Float
        , Float, Float, Float, Float ) -> M4
mat4 (a1, a2, a3, a4, b1, b2, b3, b4, c1, c2, c3, c4, d1, d2, d3, d4) =
        M4 (V4 a1 a2 a3 a4)
           (V4 b1 b2 b3 b4)
           (V4 c1 c2 c3 c4)
           (V4 d1 d2 d3 d4)

-- | 3x3 matrix multiplication.
mul3 :: M3 -> M3 -> M3
mul3 (M3 (V3 a11 a12 a13)
         (V3 a21 a22 a23)
         (V3 a31 a32 a33))
     (M3 (V3 b11 b12 b13)
         (V3 b21 b22 b23)
         (V3 b31 b32 b33)) =
        mat3 (
                a11 * b11 + a12 * b21 + a13 * b31,
                a11 * b12 + a12 * b22 + a13 * b32,
                a11 * b13 + a12 * b23 + a13 * b33,

                a21 * b11 + a22 * b21 + a23 * b31,
                a21 * b12 + a22 * b22 + a23 * b32,
                a21 * b13 + a22 * b23 + a23 * b33,

                a31 * b11 + a32 * b21 + a33 * b31,
                a31 * b12 + a32 * b22 + a33 * b32,
                a31 * b13 + a32 * b23 + a33 * b33
             )


-- | 4x4 matrix multiplication.
mul4 :: M4 -> M4 -> M4
mul4 (M4 (V4 _1 _2 _3 _4)
         (V4 _5 _6 _7 _8)
         (V4 _9 _a _b _c)
         (V4 _d _e _f _g))
     (M4 (V4 a b c d)
	 (V4 e f g h)
	 (V4 i j k l)
	 (V4 m n o p)) =
        mat4 (
                _1 * a + _2 * e + _3 * i + _4 * m,
                _1 * b + _2 * f + _3 * j + _4 * n,
                _1 * c + _2 * g + _3 * k + _4 * o,
                _1 * d + _2 * h + _3 * l + _4 * p,

                _5 * a + _6 * e + _7 * i + _8 * m,
                _5 * b + _6 * f + _7 * j + _8 * n,
                _5 * c + _6 * g + _7 * k + _8 * o,
                _5 * d + _6 * h + _7 * l + _8 * p,

                _9 * a + _a * e + _b * i + _c * m,
                _9 * b + _a * f + _b * j + _c * n,
                _9 * c + _a * g + _b * k + _c * o,
                _9 * d + _a * h + _b * l + _c * p,

                _d * a + _e * e + _f * i + _g * m,
                _d * b + _e * f + _f * j + _g * n,
                _d * c + _e * g + _f * k + _g * o,
                _d * d + _e * h + _f * l + _g * p
        )

-- | Transpose a 4x4 matrix.
transpose4 :: M4 -> M4
transpose4 (M4 (V4 a1 a2 a3 a4)
               (V4 b1 b2 b3 b4)
               (V4 c1 c2 c3 c4)
               (V4 d1 d2 d3 d4)) = M4 (V4 a1 b1 c1 d1)
                                      (V4 a2 b2 c2 d2)
                                      (V4 a3 b3 c3 d3)
                                      (V4 a4 b4 c4 d4)

-- | 4x4 identity matrix.
idMat4 :: M4
idMat4 = mat4from3 ( 1, 0, 0
                   , 0, 1, 0
                   , 0, 0, 1 )

-- | 4x4 translation matrix.
transMat4 :: V3 -> M4
transMat4 (V3 x y z) = mat4 ( 1, 0, 0, 0
                            , 0, 1, 0, 0
                            , 0, 0, 1, 0
                            , x, y, z, 1 )

-- | 4x4 rotation matrix (X axis).
rotXMat4 :: Float -> M4
rotXMat4 a = mat4from3 ( 1, 0, 0
                       , 0, cos a, - sin a
                       , 0, sin a, cos a )

-- | 4x4 rotation matrix (Y axis).
rotYMat4 :: Float -> M4
rotYMat4 a = mat4from3 ( cos a, 0, sin a
                       , 0, 1, 0
                       , - sin a, 0, cos a )

-- | 4x4 rotation matrix (Z axis).
rotZMat4 :: Float -> M4
rotZMat4 a = mat4from3 ( cos a, - sin a, 0
                       , sin a, cos a, 0
                       , 0, 0, 1 )

-- | 4x4 rotation matrix.
rotAAMat4 :: V3         -- ^ Axis.
          -> Float      -- ^ Angle
          -> M4
rotAAMat4 v = quatToMat4 . rotAAQuat v

-- | Rotation quaternion.
rotAAQuat :: V3 -> Float -> V4
rotAAQuat (V3 x y z) a = V4 (x * s) (y * s) (z * s) (cos $ a / 2)
        where s = sin $ a / 2

-- | 4x4 scale matrix.
scaleMat4 :: V3 -> M4
scaleMat4 (V3 x y z) = mat4from3 ( x, 0, 0
                                 , 0, y, 0
                                 , 0, 0, z )

-- | 4x4 perspective matrix.
perspectiveMat4 :: Float        -- ^ Far
                -> Float        -- ^ Near
                -> Float        -- ^ FOV
                -> Float        -- ^ Aspect ratio
                -> M4
perspectiveMat4 f n fov ar =
        mat4 ( s / ar , 0 , 0                 , 0
             , 0      , s , 0                 , 0
             , 0      , 0 , (f + n) / (n - f) , (2 * f * n) / (n - f)
             , 0      , 0 , - 1               , 0)
        where s = 1 / tan (fov * pi / 360)

-- | 4x4 FPS camera matrix.
cameraMat4 :: V3        -- ^ Eye
           -> Float     -- ^ Pitch
           -> Float     -- ^ Yaw
           -> M4
cameraMat4 eye pitch yaw =
        mat4 ( xx, yx, zx, 0
             , xy, yy, zy, 0
             , xz, yz, zz, 0
             , - dot3 xv eye, - dot3 yv eye, - dot3 zv eye, 1)
        where cosPitch = cos pitch
              sinPitch = sin pitch
              cosYaw = cos yaw
              sinYaw = sin yaw
              xv@(V3 xx xy xz) = V3 cosYaw 0 $ -sinYaw
              yv@(V3 yx yy yz) = V3 (sinYaw * sinPitch) cosPitch $
                                    cosYaw * sinPitch
              zv@(V3 zx zy zz) = V3 (sinYaw * cosPitch) (-sinPitch) $
                                    cosPitch * cosYaw

-- | Quaternion to 4x4 matrix.
quatToMat4 :: V4 -> M4
quatToMat4 (V4 x y z w) = mat4from3 (
        1 - 2 * y ^ 2 - 2 * z ^ 2, 2 * x * y - 2 * z * w, 2 * x * z + 2 * y * w,
        2 * x * y + 2 * z * w, 1 - 2 * x ^ 2 - 2 * z ^ 2, 2 * y * z - 2 * x * w,
        2 * x * z - 2 * y * w, 2 * y * z + 2 * x * w, 1 - 2 * x ^ 2 - 2 * y ^ 2)

-- | The identity 3x3 matrix.
idMat3 :: M3
idMat3 = mat3from2 (1, 0, 0, 1)

-- | 3x3 translation matrix.
transMat3 :: V2 -> M3
transMat3 (V2 x y) = mat3 ( 1, 0, 0
                          , 0, 1, 0
                          , x, y, 1 )

-- | 3x3 rotation matrix.
rotMat3 :: Float -> M3
rotMat3 a = mat3from2 (cos a, sin a, - sin a, cos a)

-- | 3x3 scale matrix.
scaleMat3 :: V2 -> M3
scaleMat3 (V2 x y) = mat3from2 (x, 0, 0, y)

zipWithM_ :: Monad m => (a -> b -> m c) -> [a] -> [b] -> m ()
zipWithM_ f xs = sequence_ . zipWith f xs
