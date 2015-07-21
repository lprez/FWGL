{-# LANGUAGE GADTs, MultiParamTypeClasses, DeriveDataTypeable, DataKinds,
             FunctionalDependencies #-}

-- TODO FWGL.Shader.Language.Prefix and FWGL.Shader.Prefix (or Postfix)
module FWGL.Shader.Language (
        ShaderType(..),
        Expr(..),
        Action(..),
        ContextVarType(..),
        Float(..),
        Unknown(..),
        Sampler2D(..),
        V2(..),
        V3(..),
        V4(..),
        M2(..),
        M3(..),
        M4(..),
        fromRational,
        fromInteger,
        negate,
        Mul,
        (*),
        (/),
        Sum,
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
        ifThenElse,
        loop,
        true,
        false,
        store,
        texture2D,
        radians,
        degrees,
        sin,
        cos,
        tan,
        asin,
        acos,
        atan,
        atan2,
        exp,
        log,
        exp2,
        log2,
        sqrt,
        inversesqrt,
        abs,
        sign,
        floor,
        ceil,
        fract,
        mod,
        min,
        max,
        clamp,
        mix,
        step,
        smoothstep,
        length,
        distance,
        dot,
        cross,
        normalize,
        faceforward,
        reflect,
        refract,
        matrixCompMult
        -- TODO: memoized versions of the functions
) where

import Control.Applicative
import Control.Monad
import Data.Hashable
import Data.IORef
import Data.Typeable
import Prelude (String, (.), ($), error, Maybe(..), const, fst, snd, Eq)
import qualified Prelude
import Text.Printf
import System.IO.Unsafe

-- | CPU integer.
type CInt = Prelude.Int

-- | An expression.
data Expr = Empty | Read String | Op1 String Expr | Op2 String Expr Expr
          | Apply String [Expr] | X Expr | Y Expr | Z Expr | W Expr
          | Literal String | Action Action | Dummy CInt
          | ContextVar CInt ContextVarType
          deriving Eq

-- | Expressions that have to be compiled to a statement.
data Action = Store String Expr | If Expr String Expr Expr
            | For CInt String Expr (Expr -> Expr -> (Expr, Expr))

data ContextVarType = LoopIteration | LoopValue deriving Eq

-- | A GPU boolean.
newtype Bool = Bool Expr deriving Typeable

-- | A GPU float.
newtype Float = Float Expr deriving Typeable

-- | A GPU sampler (sampler2D in GLSL).
newtype Sampler2D = Sampler2D Expr deriving Typeable

-- | The type of a generic expression.
newtype Unknown = Unknown Expr

-- | A GPU 2D vector.
-- NB: This is a different type from FWGL.Vector.'FWGL.Vector.V2'.
data V2 = V2 Float Float deriving (Typeable)

-- | A GPU 3D vector.
data V3 = V3 Float Float Float deriving (Typeable)

-- | A GPU 4D vector.
data V4 = V4 Float Float Float Float deriving (Typeable)

-- | A GPU 2x2 matrix.
data M2 = M2 V2 V2 deriving (Typeable)

-- | A GPU 3x3 matrix.
data M3 = M3 V3 V3 V3 deriving (Typeable)

-- | A GPU 4x4 matrix.
data M4 = M4 V4 V4 V4 V4 deriving (Typeable)

-- | CPU equality.
infix 4 =!
(=!) :: Prelude.Eq a => a -> a -> Prelude.Bool
(=!) = (Prelude.==)

-- | CPU and.
infixr 3 &&!
(&&!) :: Prelude.Bool -> Prelude.Bool -> Prelude.Bool
(&&!) = (Prelude.&&)

-- | A type in the GPU.
class ShaderType t where
        zero :: t

        toExpr :: t -> Expr

        fromExpr :: Expr -> t

        typeName :: t -> String

        size :: t -> CInt

instance ShaderType Unknown where
        zero = error "zero: Unknown type."
        toExpr (Unknown e) = e
        fromExpr = Unknown
        typeName = error "typeName: Unknown type."
        size = error "size: Unknown type."

instance ShaderType Bool where
        zero = Bool $ Literal "false"

        toExpr (Bool e) = e

        fromExpr = Bool

        typeName _ = "bool"

        size _ = 1

instance ShaderType Float where
        zero = Float $ Literal "0.0"

        toExpr (Float e) = e

        fromExpr = Float

        typeName _ = "float"

        size _ = 1

instance ShaderType Sampler2D where
        zero = Sampler2D $ Literal "0"

        toExpr (Sampler2D e) = e

        fromExpr = Sampler2D

        typeName _ = "sampler2D"

        size _ = 1

instance ShaderType V2 where
        zero = V2 zero zero

        toExpr (V2 (Float (X v)) (Float (Y v'))) | v =! v' = Apply "vec2" [v]
        toExpr (V2 (Float x) (Float y)) = Apply "vec2" [x, y]

        fromExpr v = V2 (Float (X v)) (Float (Y v))

        typeName _ = "vec2"

        size _ = 1

instance ShaderType V3 where
        zero = V3 zero zero zero

        toExpr (V3 (Float (X v)) (Float (Y v')) (Float (Z v'')))
               | v =! v' &&! v' =! v'' = Apply "vec3" [v]
        toExpr (V3 (Float x) (Float y) (Float z)) = Apply "vec3" [x, y, z]

        fromExpr v = V3 (Float (X v)) (Float (Y v)) (Float (Z v))

        typeName _ = "vec3"

        size _ = 1

instance ShaderType V4 where
        zero = V4 zero zero zero zero

        toExpr (V4 (Float (X v)) (Float (Y v1)) (Float (Z v2)) (Float (W v3)))
               | v =! v1 &&! v1 =! v2 &&! v2 =! v3 = Apply "vec4" [v]
        toExpr (V4 (Float x) (Float y) (Float z) (Float w)) =
                Apply "vec4" [x, y, z, w]

        fromExpr v = V4 (Float (X v)) (Float (Y v)) (Float (Z v)) (Float (W v))

        typeName _ = "vec4"

        size _ = 1

instance ShaderType M2 where
        zero = M2 zero zero

        toExpr (M2 (V2 (Float (X (X m))) (Float (X (Y m1))))
                   (V2 (Float (Y (X m2))) (Float (Y (Y m3)))))
               | m =! m1 &&! m1 =! m2 &&! m2 =! m3 = Apply "mat2" [m]
        toExpr (M2 (V2 (Float xx) (Float xy))
                   (V2 (Float yx) (Float yy)))
               = Apply "mat2" [xx, yx, xy, yy]

        fromExpr m = M2 (V2 (Float (X (X m))) (Float (Y (X m))))
                        (V2 (Float (Y (X m))) (Float (Y (Y m))))

        typeName _ = "mat2"

        size _ = 2

instance ShaderType M3 where
        zero = M3 zero zero zero

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
                 m4 =! m5 &&! m5 =! m6 &&! m6 =! m7 &&! m7 =! m8 =
                         Apply "mat3" [m]
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
        zero = M4 zero zero zero zero

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
                 m12 =! m13 &&! m13 =! m14 &&! m14 =! m15 = Apply "mat4" [m]
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

class ShaderType a => Vector a
instance Vector V2
instance Vector V3
instance Vector V4

class ShaderType a => Matrix a
instance Matrix M2
instance Matrix M3
instance Matrix M4

-- | Types that can be multiplied.
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

-- | Floats or vectors.
class ShaderType a => GenType a
instance GenType Float
instance GenType V2
instance GenType V3
instance GenType V4

infixl 7 *
(*) :: (Mul a b c, ShaderType a, ShaderType b, ShaderType c) => a -> b -> c
x * y = fromExpr $ Op2 "*" (toExpr x) (toExpr y)

infixl 7 /
(/) :: (Mul a b c, ShaderType a, ShaderType b, ShaderType c) => a -> b -> c
x / y = fromExpr $ Op2 "/" (toExpr x) (toExpr y)

-- | Types that can be added.
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

-- TODO: not

negate :: Float -> Float
negate (Float e) = Float $ Op1 "-" e

fromInteger :: Prelude.Integer -> Float -- Integer
fromInteger = fromRational . Prelude.fromIntegral

fromRational :: Prelude.Rational -> Float
fromRational = Float . Literal
                        . (printf "%f" :: Prelude.Float -> String)
                        . Prelude.fromRational

radians :: GenType a => a -> a
radians x = fromExpr $ Apply "radians" [toExpr x]

degrees :: GenType a => a -> a
degrees x = fromExpr $ Apply "degrees" [toExpr x]

sin :: GenType a => a -> a
sin x = fromExpr $ Apply "sin" [toExpr x]

cos :: GenType a => a -> a
cos x = fromExpr $ Apply "cos" [toExpr x]

tan :: GenType a => a -> a
tan x = fromExpr $ Apply "tan" [toExpr x]

asin :: GenType a => a -> a
asin x = fromExpr $ Apply "asin" [toExpr x]

acos :: GenType a => a -> a
acos x = fromExpr $ Apply "acos" [toExpr x]

atan :: GenType a => a -> a
atan x = fromExpr $ Apply "atan" [toExpr x]

atan2 :: GenType a => a -> a -> a
atan2 x y = fromExpr $ Apply "atan" [toExpr x, toExpr y]

exp :: GenType a => a -> a
exp x = fromExpr $ Apply "exp" [toExpr x]

log :: GenType a => a -> a
log x = fromExpr $ Apply "log" [toExpr x]

exp2 :: GenType a => a -> a
exp2 x = fromExpr $ Apply "exp2" [toExpr x]

log2 :: GenType a => a -> a
log2 x = fromExpr $ Apply "log2" [toExpr x]

sqrt :: GenType a => a -> a
sqrt x = fromExpr $ Apply "sqrt" [toExpr x]

inversesqrt :: GenType a => a -> a
inversesqrt x = fromExpr $ Apply "inversesqrt" [toExpr x]

abs :: GenType a => a -> a
abs x = fromExpr $ Apply "abs" [toExpr x]

sign :: GenType a => a -> a
sign x = fromExpr $ Apply "sign" [toExpr x]

floor :: GenType a => a -> a
floor x = fromExpr $ Apply "floor" [toExpr x]

ceil :: GenType a => a -> a
ceil x = fromExpr $ Apply "ceil" [toExpr x]

fract :: GenType a => a -> a
fract x = fromExpr $ Apply "fract" [toExpr x]

mod :: (GenType a, GenType b) => a -> b -> a
mod x y = fromExpr $ Apply "mod" [toExpr x, toExpr y]

min :: GenType a => a -> a -> a
min x y = fromExpr $ Apply "min" [toExpr x, toExpr y]

max :: GenType a => a -> a -> a
max x y = fromExpr $ Apply "max" [toExpr x, toExpr y]

clamp :: (GenType a, GenType b) => a -> b -> b -> a
clamp x y z = fromExpr $ Apply "clamp" [toExpr x, toExpr y, toExpr z]

mix :: (GenType a, GenType b) => a -> a -> b -> a
mix x y z = fromExpr $ Apply "mix" [toExpr x, toExpr y, toExpr z]

step :: GenType a => a -> a -> a
step x y = fromExpr $ Apply "step" [toExpr x, toExpr y]

smoothstep :: (GenType a, GenType b) => b -> b -> a -> a
smoothstep x y z = fromExpr $ Apply "smoothstep" [toExpr x, toExpr y, toExpr z]

length :: GenType a => a -> Float
length x = fromExpr $ Apply "length" [toExpr x]

distance :: GenType a => a -> a -> Float
distance x y = fromExpr $ Apply "distance" [toExpr x, toExpr y]

dot :: GenType a => a -> a -> Float
dot x y = fromExpr $ Apply "dot" [toExpr x, toExpr y]

cross :: V3 -> V3 -> V3
cross x y = fromExpr $ Apply "cross" [toExpr x, toExpr y]

normalize :: GenType a => a -> a
normalize x = fromExpr $ Apply "normalize" [toExpr x]

faceforward :: GenType a => a -> a -> a -> a
faceforward x y z = fromExpr $ Apply "faceforward" [toExpr x, toExpr y, toExpr z]

reflect :: GenType a => a -> a -> a
reflect x y = fromExpr $ Apply "reflect" [toExpr x, toExpr y]

refract :: GenType a => a -> a -> Float -> a
refract x y z = fromExpr $ Apply "refract" [toExpr x, toExpr y, toExpr z]

-- TODO: unsafe
matrixCompMult :: (Matrix a, Matrix b, Matrix c) => a -> b -> c
matrixCompMult x y = fromExpr $ Apply "matrixCompMult" [toExpr x, toExpr y]


-- TODO: add functions, ifThenElse, etc.

-- | Avoid executing this expression more than one time. Conditionals and loops
-- imply it.
store :: ShaderType a => a -> a
store x = fromExpr . Action $ Store (typeName x) (toExpr x)

true :: Bool
true = Bool $ Literal "true"

false :: Bool
false = Bool $ Literal "false"

-- | Rebinded if.
ifThenElse :: ShaderType a => Bool -> a -> a -> a
ifThenElse b t f = fromExpr . Action $ If (toExpr b) (typeName t)
                                          (toExpr t) (toExpr f)

loop :: ShaderType a 
     => Float -- ^ Maximum number of iterations (should be as low as possible, must be an integer literal)
     -> a -- ^ Initial value
     -> (Float -> a -> (a, Bool)) -- ^ Iteration -> Old value -> (Next, Stop)
     -> a
loop (Float (Literal iters)) iv f =
        fromExpr . Action $
                For (Prelude.floor (Prelude.read iters :: Prelude.Float))
                    (typeName iv)
                    (toExpr iv)
                    (\ie ve -> let (next, stop) = f (fromExpr ie) (fromExpr ve)
                               in (toExpr next, toExpr stop))
loop _ _ _ = error "loop: iteration number is not a literal."

texture2D :: Sampler2D -> V2 -> V4
texture2D (Sampler2D s) v = fromExpr $ Apply "texture2D" [s, toExpr v]

instance Hashable Expr where
        hashWithSalt s e = case e of
                                Empty -> hash2 s 0 (0 :: CInt)
                                Read str -> hash2 s 1 str
                                Op1 str exp -> hash2 s 2 (str, exp)
                                Op2 str exp exp' -> hash2 3 s (str, exp, exp')
                                Apply str exps -> hash2 4 s exps
                                X exp -> hash2 5 s exp
                                Y exp -> hash2 6 s exp
                                Z exp -> hash2 7 s exp
                                W exp -> hash2 8 s exp
                                Literal str -> hash2 s 9 str
                                Action hash -> hash2 s 10 hash
                                Dummy i -> hash2 s 11 i
                                ContextVar i LoopIteration -> hash2 s 12 i
                                ContextVar i LoopValue -> hash2 s 13 i

instance Hashable Action where
        hashWithSalt s (Store t e) = hash2 s 0 (t, e)
        hashWithSalt s (If eb tt et ef) = hash2 s 1 (eb, tt, et, ef)
        hashWithSalt s (For iters tv iv eFun) =
                let baseHash = hash (iters, tv, iv, eFun (Dummy 0) (Dummy 1))
                in hash2 s 2 ( baseHash
                             , eFun (Dummy baseHash)
                                    (Dummy $ baseHash Prelude.+ 1))

instance Prelude.Eq Action where
        a == a' = hash a =! hash a'

hash2 :: Hashable a => CInt -> CInt -> a -> CInt
hash2 s i x = s `hashWithSalt` i `hashWithSalt` x
