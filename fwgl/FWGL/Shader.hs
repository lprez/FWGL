{-# LANGUAGE FlexibleContexts, RankNTypes, TypeFamilies #-}

{-|
An example of shader variable:

@
        newtype Transform2 = Transform2 M3
                deriving (Typeable,-- This have a name in the shader.
                          ShaderType, -- This is a type in the GPU (3x3 matrix).
                          UniformCPU CM3) -- This can be used as an uniform
                                             and you can set it using a CPU
                                             3x3 matrix
                                             (FWGL.Vector.'FWGL.Vector.M3').
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
                     (Position2 (V2 x y) :- uv@(UV _) :- N) =
        -- Matrix and vector multiplication:
                        let V3 x' y' _ = view * trans * V3 x y 1
        -- Set of outputs:
                        in Vertex (V4 x' y' z 1) -- Vertex position.
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
        V2(..),
        V3(..),
        V4(..),
        M2(..),
        M3(..),
        M4(..),
        CFloat,
        CSampler2D,
        CV2,
        CV3,
        CV4,
        CM2,
        CM3,
        CM4,
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
import qualified FWGL.Internal.GL as CPU
import FWGL.Shader.CPU (UniformCPU, AttributeCPU)
import FWGL.Shader.Language
import FWGL.Shader.Shader
import FWGL.Shader.Stages
import qualified FWGL.Vector as CPU
import Prelude ((.), id, const, flip, ($))
import qualified Prelude as CPU

-- | Floats in the CPU.
type CFloat = CPU.Float

-- | Samplers in the CPU.
type CSampler2D = CPU.ActiveTexture

-- | 2D vectors in the CPU.
type CV2 = CPU.V2

-- | 3D vectors in the CPU.
type CV3 = CPU.V3

-- | 4D vectors in the CPU.
type CV4 = CPU.V4

-- | 2x2 matrices in the CPU.
type CM2 = CPU.M2

-- | 3x3 matrices in the CPU.
type CM3 = CPU.M3

-- | 4x4 matrices in the CPU.
type CM4 = CPU.M4
