{-# LANGUAGE FlexibleContexts, RankNTypes, TypeFamilies #-}

{-|
An example of shader variable:

@
        newtype Transform2 = Transform2 Mat3
                deriving (Typeable,
                          ShaderType, -- This is a type in the GPU (3x3 matrix).
                          UniformCPU CMat3) -- This can be used as an uniform
                                            -- and you can set it using a CPU
                                            -- 3x3 matrix
                                            -- (FWGL.Vector.'FWGL.Vector.Mat3')
@

An example of vertex shader:

@
        vertexShader :: VertexShader
        -- The types of the uniforms:
                                '[Transform2, View2, Depth]
        -- The types of the attributes:
                                '[Position2, UV]
        -- The types of the varying (outputs), excluding 'VertexShaderOutput'.
                                '[UV]
        vertexShader 
        -- Set of uniforms:
                     (Transform2 trans :- View2 view :- Depth z :- N)
        -- Set of attributes:
                     (Position2 (Vec2 x y) :- uv@(UV _) :- N) =
        -- Matrix and vector multiplication:
                        let Vec3 x' y' _ = view * trans * Vec3 x y 1
        -- Set of outputs:
                        in Vertex (Vec4 x' y' z 1) -- Vertex position.
                           :- uv :- N
@

Required extensions:

@
\{\-# LANGUAGE DataKinds, RebindableSyntax, DeriveDataTypeable,
             GeneralizedNewtypeDeriving, GADTs #\-\}
@

-}

module FWGL.Shader (
        Shader,
        VertexShader,
        FragmentShader,
        VertexShaderOutput(Vertex),
        FragmentShaderOutput(Fragment),
        Typeable,
        AllTypeable,
        ShaderType,
        UniformCPU,
        AttributeCPU,
        Float,
        Sampler2D,
        Vec2(..),
        Vec3(..),
        Vec4(..),
        Mat2(..),
        Mat3(..),
        Mat4(..),
        CFloat,
        CSampler2D,
        CVec2,
        CVec3,
        CVec4,
        CMat2,
        CMat3,
        CMat4,
        negate,
        fromInteger,
        fromRational,
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
        matrixCompMult,
        position,
        fragColor,
        STList((:-), N),
        (.),
        id,
        const,
        flip,
        ($),
        CPU.fst,
        CPU.snd
) where

import Data.Typeable (Typeable)
import qualified Data.Vect.Float as CPU
import qualified FWGL.Internal.GL as CPU
import FWGL.Shader.CPU
import FWGL.Shader.Language
import FWGL.Shader.Shader
import FWGL.Shader.Stages
import Prelude ((.), id, const, flip, ($))
import qualified Prelude as CPU

-- | Floats in the CPU.
type CFloat = CPU.Float

-- | Samplers in the CPU.
type CSampler2D = CPU.ActiveTexture

-- | 2D vectors in the CPU.
type CVec2 = CPU.Vec2

-- | 3D vectors in the CPU.
type CVec3 = CPU.Vec3

-- | 4D vectors in the CPU.
type CVec4 = CPU.Vec4

-- | 2x2 matrices in the CPU.
type CMat2 = CPU.Mat2

-- | 3x3 matrices in the CPU.
type CMat3 = CPU.Mat3

-- | 4x4 matrices in the CPU.
type CMat4 = CPU.Mat4
