{-# LANGUAGE DataKinds, RebindableSyntax, DeriveDataTypeable,
             GeneralizedNewtypeDeriving, GADTs #-}

module Shaders where

import FWGL.Shader
import FWGL.Shader.Default3D

type PickUniforms = '[View3, Transform3, ObjectId]
type PickAttributes = Attributes

newtype ObjectId = ObjectId Vec3
        deriving (Typeable, ShaderType, UniformCPU CVec3)

pickVertexShader :: VertexShader '[Transform3, View3] PickAttributes '[]
pickVertexShader (Transform3 modelMatrix :- View3 viewMatrix :- N)
                 (Position3 (Vec3 x y z) :- _ :- _ :- N) =
             let v = viewMatrix * modelMatrix * Vec4 x y z 1.0
             in Vertex v :- N

pickFragmentShader :: FragmentShader '[ ObjectId ] '[]
pickFragmentShader (ObjectId (Vec3 ix iy iz) :- N) N =
                   Fragment (Vec4 ix iy iz 255 / 255) :- N
