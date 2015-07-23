{-# LANGUAGE DataKinds, RebindableSyntax, DeriveDataTypeable, FlexibleContexts,
             GeneralizedNewtypeDeriving, GADTs #-}

module Program (
        vertexShader,
        fragmentShader,
        Pointer,
        Uniforms,
        Attributes
) where

import FWGL.Shader
import FWGL.Shader.Default2D hiding (vertexShader, Uniforms, Attributes)

newtype Pointer = Pointer V2
        deriving (Typeable, ShaderType, UniformCPU CV2)

type VUniforms = '[Pointer, Transform2, View2, Depth]
type Uniforms = '[Pointer, Transform2, View2, Depth, Image]
type Attributes = '[Position2, UV]

vertexShader :: VertexShader VUniforms Attributes '[UV]
vertexShader (Pointer (V2 px py) :- Transform2 trans :-
              View2 view :- Depth z :- N)
             (Position2 (V2 x y) :- uv@(UV _) :- N) =
                let V3 x' y' _ = view * trans * V3 x y 1
                    dist x x' = x + (abs $ x' - x) ^ 0.7 / 2 * sign (x - x')
                in Vertex (V4 (dist x' px) (dist y' py) z 1) :- uv :- N
