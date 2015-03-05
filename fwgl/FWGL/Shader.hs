{-# LANGUAGE FlexibleContexts, RankNTypes, TypeFamilies #-}

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
        abs,
        sign,
        sqrt,
        texture2D,
        STList((:-), N),
        {-
        (>>=),
        (>>),
        fail,
        return,
        get,
        global,
        put,
        putVertex,
        putFragment,
        -}
        (.),
        id,
        const,
        flip,
        ($)
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

type CFloat = CPU.Float
type CSampler2D = CPU.ActiveTexture
type CV2 = CPU.V2
type CV3 = CPU.V3
type CV4 = CPU.V4
type CM2 = CPU.M2
type CM3 = CPU.M3
type CM4 = CPU.M4
