{-# LANGUAGE GeneralizedNewtypeDeriving, DeriveDataTypeable, DataKinds,
             FlexibleContexts, RebindableSyntax, TypeFamilies #-}

module FWGL.Shader.Default2D where

import FWGL.Shader

type Uniforms = '[View2, Image, Depth, Transform2]
type Attributes = '[Position2]

newtype Image = Image Sampler2D
        deriving (Typeable, ShaderType, UniformCPU CSampler2D)

newtype Depth = Depth Float
        deriving (Typeable, ShaderType, UniformCPU CFloat)

newtype Transform2 = Transform2 M3
        deriving (Typeable, ShaderType, UniformCPU CM3)

newtype View2 = View2 M3
        deriving (Typeable, ShaderType, UniformCPU CM3)

newtype Position2 = Position2 V2
        deriving (Typeable, ShaderType, AttributeCPU CV2)

vertexShader :: VertexShader '[Transform2, View2, Depth]
                             '[Position2] '[Position2]
vertexShader = do p2@(Position2 (V2 x y)) <- get
                  Transform2 trans <- global
                  View2 view <- global
                  Depth z <- global
                  let V3 x' y' _ = view * trans * (V3 x y 1)
                  putVertex $ V4 x' y' z 1
                  put p2

fragmentShader :: FragmentShader '[Image] '[Position2]
fragmentShader = do Image sampler <- global
                    Position2 uv <- get
                    putFragment $ texture2D sampler uv
