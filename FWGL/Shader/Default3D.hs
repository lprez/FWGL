{-# LANGUAGE GeneralizedNewtypeDeriving, DeriveDataTypeable, DataKinds,
             FlexibleContexts, RebindableSyntax, TypeFamilies #-}

module FWGL.Shader.Default3D where

import FWGL.Shader.CPU
import FWGL.Shader
import qualified FWGL.Vector

type Uniforms = '[Transform3, View3, Texture2]
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
                             '[ UV ]
vertexShader = do v <- applyMatrices
                  get >>= \x@(UV _) -> put x
                  (Normal3 _) <- get
                  putVertex v

applyMatrices :: Shader '[ Transform3, View3 ]
                        '[ Position3 ]
                        '[]
                        V4
applyMatrices = do View3 viewMatrix <- global
                   Transform3 modelMatrix <- global
                   Position3 (V3 x y z) <- get
                   return $
                        viewMatrix * modelMatrix * V4 x y z 1.0

fragmentShader :: FragmentShader '[ Texture2 ] [ UV, Normal3 ]
fragmentShader = do Texture2 sampler <- global
                    UV (V2 s t) <- get
                    putFragment .
                            texture2D sampler $ V2 s (1 - t)
