{-# LANGUAGE GeneralizedNewtypeDeriving, DeriveDataTypeable, DataKinds,
             FlexibleContexts, RebindableSyntax, TypeFamilies #-}

module Program (
        vertexShader,
        fragmentShader,
        Pointer
) where

import FWGL.Shader
import FWGL.Shader.Default2D hiding (vertexShader)

newtype Pointer = Pointer V2
        deriving (Typeable, ShaderType, UniformCPU CV2)

vertexShader :: VertexShader '[Pointer, Transform2, View2, Depth]
                             '[Position2, UV] '[UV]
vertexShader = do (Position2 (V2 x y)) <- get
                  uv@(UV _) <- get

                  Transform2 trans <- global
                  View2 view <- global
                  Depth z <- global
                  Pointer (V2 px py) <- global

                  let V3 x' y' _ = view * trans * V3 x y 1
                  putVertex $ V4 (dist x' px)
                                 (dist y' py) z 1
                  put uv

        where dist x x' = x + (abs $ x' - x) ^ 0.7 / 2 * sign (x - x')
