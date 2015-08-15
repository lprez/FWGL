{-# LANGUAGE DataKinds, RebindableSyntax, DeriveDataTypeable, FlexibleContexts,
             GeneralizedNewtypeDeriving, GADTs #-}

module Program (
        vertexShader,
        fragmentShader,
        Pointer(..),
        Uniforms,
        Attributes
) where

import FWGL.Shader
import FWGL.Shader.Default2D hiding (vertexShader, Uniforms, Attributes)

newtype Pointer = Pointer Vec2
        deriving (Typeable, ShaderType, UniformCPU CVec2)

type VUniforms = '[Pointer, Transform2, View2, Depth]
type Uniforms = '[Pointer, Transform2, View2, Depth, Image]
type Attributes = '[Position2, UV]

vertexShader :: VertexShader VUniforms Attributes '[UV]
vertexShader (Pointer (Vec2 px py) :- Transform2 trans :-
              View2 view :- Depth z :- N)
             (Position2 (Vec2 x y) :- uv@(UV _) :- N) =
                let Vec3 x' y' _ = store $ view * trans * Vec3 x y 1
                    dist x x' = x + (abs $ x' - x) ^ 0.7 / 2 * sign (x - x')
                in Vertex (Vec4 (dist x' px) (dist y' py) z 1) :- uv :- N
