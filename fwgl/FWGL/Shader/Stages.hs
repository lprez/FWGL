{-# LANGUAGE TypeOperators, DataKinds, GeneralizedNewtypeDeriving,
             DeriveDataTypeable, RankNTypes, FlexibleContexts,
             TypeFamilies, ConstraintKinds #-}

module FWGL.Shader.Stages (
        VertexShader,
        ValidVertex,
        FragmentShader,
        VertexShaderOutput(Vertex),
        FragmentShaderOutput(Fragment)
) where

import Data.Typeable

import FWGL.Internal.TList
import FWGL.Shader.Language
import FWGL.Shader.Shader

type VertexShader g i o = Shader g i (VertexShaderOutput ': o)
type FragmentShader g i = Shader g i (FragmentShaderOutput ': '[])

type ValidVertex g i o = (Valid g i o, IsMember VertexShaderOutput o ~ False)

newtype VertexShaderOutput = Vertex V4 deriving (Typeable, ShaderType)
newtype FragmentShaderOutput = Fragment V4 deriving (Typeable, ShaderType)
