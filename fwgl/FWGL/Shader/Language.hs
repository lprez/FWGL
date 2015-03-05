{-# LANGUAGE GADTs, MultiParamTypeClasses, DeriveDataTypeable, DataKinds,
             FunctionalDependencies #-}

module FWGL.Shader.Language (
        ShaderType(..),
        Expr(..),
        Float(..),
        Sampler2D(..),
        V2(..),
        V3(..),
        V4(..),
        M2(..),
        M3(..),
        M4(..),
        -- (==), (>=, >, ..), ifThenElse
        fromRational,
        fromInteger,
        negate,
        (*),
        (/),
        (+),
        (-),
        (^),
        (&&),
        (||),
        (==),
        (>=),
        (<=),
        (<),
        (>),
        abs,
        sign,
        texture2D,
        sqrt,
) where

import Data.Typeable
import Prelude (String, (.), ($))
import qualified Prelude
import Text.Printf

data Expr = Empty | Read String | Op1 String Expr | Op2 String Expr Expr
          | Apply String [Expr] | X Expr | Y Expr | Z Expr | W Expr
          | Literal String deriving (Prelude.Eq)

newtype Bool = Bool Expr deriving Typeable
newtype Float = Float Expr deriving Typeable
newtype Sampler2D = Sampler2D Expr deriving Typeable
-- | NB: These are different types from 'FWGL.Vector.V2', 'FWGL.Vector.V3', etc.
data V2 = V2 Float Float deriving (Typeable)
data V3 = V3 Float Float Float deriving (Typeable)
data V4 = V4 Float Float Float Float deriving (Typeable)
data M2 = M2 V2 V2 deriving (Typeable)
data M3 = M3 V3 V3 V3 deriving (Typeable)
data M4 = M4 V4 V4 V4 V4 deriving (Typeable)

infix 4 =!
(=!) :: Prelude.Eq a => a -> a -> Prelude.Bool
(=!) = (Prelude.==)

infixr 3 &&!
(&&!) :: Prelude.Bool -> Prelude.Bool -> Prelude.Bool
(&&!) = (Prelude.&&)

class ShaderType t where
        toExpr :: t -> Expr

        fromExpr :: Expr -> t

        typeName :: t -> String

        size :: t -> Prelude.Int

instance ShaderType Bool where
        toExpr (Bool e) = e

        fromExpr = Bool

        typeName _ = "bool"

        size _ = 1

instance ShaderType Float where
        toExpr (Float e) = e

        fromExpr = Float

        typeName _ = "float"

        size _ = 1

instance ShaderType Sampler2D where
        toExpr (Sampler2D e) = e

        fromExpr = Sampler2D

        typeName _ = "sampler2D"

        size _ = 1

instance ShaderType V2 where
        toExpr (V2 (Float (X v)) (Float (Y v'))) | v =! v' = v
        toExpr (V2 (Float x) (Float y)) = Apply "vec2" [x, y]

        fromExpr v = V2 (Float (X v)) (Float (Y v))

        typeName _ = "vec2"

        size _ = 1

instance ShaderType V3 where
        toExpr (V3 (Float (X v)) (Float (Y v')) (Float (Z v'')))
               | v =! v' &&! v' =! v'' = v
        toExpr (V3 (Float x) (Float y) (Float z)) = Apply "vec3" [x, y, z]

        fromExpr v = V3 (Float (X v)) (Float (Y v)) (Float (Z v))

        typeName _ = "vec3"

        size _ = 1

instance ShaderType V4 where
        toExpr (V4 (Float (X v)) (Float (Y v1)) (Float (Z v2)) (Float (W v3)))
               | v =! v1 &&! v1 =! v2 &&! v2 =! v3 = v
        toExpr (V4 (Float x) (Float y) (Float z) (Float w)) =
                Apply "vec4" [x, y, z, w]

        fromExpr v = V4 (Float (X v)) (Float (Y v)) (Float (Z v)) (Float (W v))

        typeName _ = "vec4"

        size _ = 1

instance ShaderType M2 where
        toExpr (M2 (V2 (Float (X (X m))) (Float (X (Y m1))))
                   (V2 (Float (Y (X m2))) (Float (Y (Y m3)))))
               | m =! m1 &&! m1 =! m2 &&! m2 =! m3 = m
        toExpr (M2 (V2 (Float xx) (Float xy))
                   (V2 (Float yx) (Float yy)))
               = Apply "mat2" [xx, yx, xy, yy]

        fromExpr m = M2 (V2 (Float (X (X m))) (Float (Y (X m))))
                        (V2 (Float (Y (X m))) (Float (Y (Y m))))

        typeName _ = "mat2"

        size _ = 2

instance ShaderType M3 where
        toExpr (M3 (V3 (Float (X (X m)))
                       (Float (X (Y m1)))
                       (Float (X (Z m2))))
                   (V3 (Float (Y (X m3)))
                       (Float (Y (Y m4)))
                       (Float (Y (Z m5))))
                   (V3 (Float (Z (X m6)))
                       (Float (Z (Y m7)))
                       (Float (Z (Z m8)))))
               | m =! m1 &&! m1 =! m2 &&! m2 =! m3 &&! m3 =! m4 &&!
                 m4 =! m5 &&! m5 =! m6 &&! m6 =! m7 &&! m7 =! m8 = m
        toExpr (M3 (V3 (Float xx) (Float xy) (Float xz))
                   (V3 (Float yx) (Float yy) (Float yz))
                   (V3 (Float zx) (Float zy) (Float zz)))
               = Apply "mat3" [xx, yx, zx, xy, yy, zy, xz, yz, zz]

        fromExpr m = M3 (V3 (Float (X (X m)))
                            (Float (X (Y m)))
                            (Float (X (Z m))))
                        (V3 (Float (Y (X m)))
                            (Float (Y (Y m)))
                            (Float (Y (Z m))))
                        (V3 (Float (Z (X m)))
                            (Float (Z (Y m)))
                            (Float (Z (Z m))))

        typeName _ = "mat3"

        size _ = 3

instance ShaderType M4 where
        toExpr (M4 (V4 (Float (X (X m)))
                       (Float (X (Y m1)))
                       (Float (X (Z m2)))
                       (Float (X (W m3))))
                   (V4 (Float (Y (X m4)))
                       (Float (Y (Y m5)))
                       (Float (Y (Z m6)))
                       (Float (Y (W m7))))
                   (V4 (Float (Z (X m8)))
                       (Float (Z (Y m9)))
                       (Float (Z (Z m10)))
                       (Float (Z (W m11))))
                   (V4 (Float (W (X m12)))
                       (Float (W (Y m13)))
                       (Float (W (Z m14)))
                       (Float (W (W m15)))))
               | m =! m1 &&! m1 =! m2 &&! m2 =! m3 &&! m3 =! m4 &&!
                 m4 =! m5 &&! m5 =! m6 &&! m6 =! m7 &&! m7 =! m8 &&!
                 m8 =! m9 &&! m9 =! m10 &&! m10 =! m11 &&! m11 =! m12 &&!
                 m12 =! m13 &&! m13 =! m14 &&! m14 =! m15 = m
        toExpr (M4 (V4 (Float xx) (Float xy) (Float xz) (Float xw))
                   (V4 (Float yx) (Float yy) (Float yz) (Float yw))
                   (V4 (Float zx) (Float zy) (Float zz) (Float zw))
                   (V4 (Float wx) (Float wy) (Float wz) (Float ww)))
               = Apply "mat4" [ xx, yx, zx, wx
                              , xy, yy, zy, wy
                              , xz, yz, zz, wz
                              , xw, yw, zw, ww ]

        fromExpr m = M4 (V4 (Float (X (X m)))
                            (Float (X (Y m)))
                            (Float (X (Z m)))
                            (Float (X (W m))))
                        (V4 (Float (Y (X m)))
                            (Float (Y (Y m)))
                            (Float (Y (Z m)))
                            (Float (Y (W m))))
                        (V4 (Float (Z (X m)))
                            (Float (Z (Y m)))
                            (Float (Z (Z m)))
                            (Float (Z (W m))))
                        (V4 (Float (W (X m)))
                            (Float (W (Y m)))
                            (Float (W (Z m)))
                            (Float (W (W m))))

        typeName _ = "mat4"

        size _ = 4

class Vector a
instance Vector V2
instance Vector V3
instance Vector V4

class Matrix a
instance Matrix M2
instance Matrix M3
instance Matrix M4

class Mul a b c | a b -> c
instance Mul Float Float Float
instance Mul V2 V2 V2
instance Mul V3 V3 V3
instance Mul V4 V4 V4
instance Mul V2 Float V2
instance Mul V3 Float V3
instance Mul V4 Float V4
instance Mul Float V2 V2
instance Mul Float V3 V3
instance Mul Float V4 V4
instance Mul M2 M2 M2
instance Mul M3 M3 M3
instance Mul M4 M4 M4
instance Mul M2 Float M2
instance Mul M3 Float M3
instance Mul M4 Float M4
instance Mul Float M2 M2
instance Mul Float M3 M3
instance Mul Float M4 M4
instance Mul M2 V2 V2
instance Mul M3 V3 V3
instance Mul M4 V4 V4
instance Mul V2 M2 V2
instance Mul V3 M3 V3
instance Mul V4 M4 V4

infixl 7 *
(*) :: (Mul a b c, ShaderType a, ShaderType b, ShaderType c) => a -> b -> c
x * y = fromExpr $ Op2 "*" (toExpr x) (toExpr y)

infixl 7 /
(/) :: (Mul a b c, ShaderType a, ShaderType b, ShaderType c) => a -> b -> c
x / y = fromExpr $ Op2 "/" (toExpr x) (toExpr y)

class Sum a
instance Sum Float
instance Sum V2
instance Sum V3
instance Sum V4
instance Sum M2
instance Sum M3
instance Sum M4

infixl 6 +
(+) :: (Sum a, ShaderType a) => a -> a -> a
x + y = fromExpr $ Op2 "+" (toExpr x) (toExpr y)

infixl 6 -
(-) :: (Sum a, ShaderType a) => a -> a -> a
x - y = fromExpr $ Op2 "-" (toExpr x) (toExpr y)

infixr 8 ^
-- TODO: type-unsafe?
(^) :: (ShaderType a, ShaderType b) => a -> b -> a
x ^ y = fromExpr $ Apply "pow" [toExpr x, toExpr y]

infixr 3 &&
(&&) :: Bool -> Bool -> Bool
x && y = fromExpr $ Op2 "&&" (toExpr x) (toExpr y)

infixr 2 ||
(||) :: Bool -> Bool -> Bool
x || y = fromExpr $ Op2 "||" (toExpr x) (toExpr y)

infix 4 ==
(==) :: ShaderType a => a -> a -> Bool
x == y = fromExpr $ Op2 "==" (toExpr x) (toExpr y)

infix 4 /=
(/=) :: ShaderType a => a -> a -> Bool
x /= y = fromExpr $ Op2 "!=" (toExpr x) (toExpr y)

infix 4 >=
(>=) :: ShaderType a => a -> a -> Bool
x >= y = fromExpr $ Op2 ">=" (toExpr x) (toExpr y)

infix 4 <=
(<=) :: ShaderType a => a -> a -> Bool
x <= y = fromExpr $ Op2 "<=" (toExpr x) (toExpr y)

infix 4 <
(<) :: ShaderType a => a -> a -> Bool
x < y = fromExpr $ Op2 "<" (toExpr x) (toExpr y)

infix 4 >
(>) :: ShaderType a => a -> a -> Bool
x > y = fromExpr $ Op2 ">" (toExpr x) (toExpr y)

negate :: Float -> Float
negate (Float e) = Float $ Op1 "-" e

fromInteger :: Prelude.Integer -> Float -- Integer
fromInteger = fromRational . Prelude.fromIntegral

fromRational :: Prelude.Rational -> Float
fromRational = Float . Literal
                        . (printf "%f" :: Prelude.Float -> String)
                        . Prelude.fromRational

abs :: Float -> Float
abs (Float e) = Float $ Apply "abs" [e]

sign :: Float -> Float
sign (Float e) = Float $ Apply "sign" [e]

-- TODO: add functions, ifThenElse, etc.

sqrt :: Float -> Float
sqrt (Float e) = Float $ Apply "sqrt" [e]

texture2D :: Sampler2D -> V2 -> V4
texture2D (Sampler2D s) v = fromExpr $ Apply "texture2D" [s, toExpr v]
