{-# LANGUAGE DataKinds, RebindableSyntax, DeriveDataTypeable,
             GeneralizedNewtypeDeriving, GADTs #-}

module FWGL.Shader.Default3D where

import FWGL.Shader
import qualified FWGL.Vector

type Uniforms = '[View3, Transform3, Texture2]
type Attributes = '[Position3, UV, Normal3]

newtype Texture2 = Texture2 Sampler2D
        deriving (Typeable, ShaderType, UniformCPU CSampler2D)

newtype Transform3 = Transform3 M4
        deriving (Typeable, ShaderType, UniformCPU CM4)

newtype View3 = View3 M4
        deriving (Typeable, ShaderType, UniformCPU CM4)

newtype Position3 = Position3 V3
        deriving (Typeable, ShaderType, AttributeCPU CV3)

newtype Normal3 = Normal3 V3
        deriving (Typeable, ShaderType, AttributeCPU CV3)

newtype UV = UV V2
        deriving (Typeable, ShaderType, AttributeCPU CV2)

vertexShader :: VertexShader '[ Transform3, View3 ]
                             '[ Position3, UV, Normal3 ]
                             '[ UV, Normal3 ]
vertexShader (Transform3 modelMatrix :- View3 viewMatrix :- N)
             (Position3 (V3 x y z) :- uv@(UV _) :- norm@(Normal3 _) :- N) =
             let v = viewMatrix * modelMatrix * V4 x y z 1.0
             in Vertex v :- uv :- norm :- N

fragmentShader :: FragmentShader '[ Texture2 ] [ UV, Normal3 ]
fragmentShader (Texture2 sampler :- N) (UV (V2 s t) :- Normal3 _ :- N) =
                Fragment (texture2D sampler $ V2 s (1 - t)) :- N
