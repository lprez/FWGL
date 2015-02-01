{-# LANGUAGE TypeOperators, DataKinds, GeneralizedNewtypeDeriving,
             DeriveDataTypeable, RankNTypes, FlexibleContexts #-}

module FWGL.Shader.Stages (
        VertexShader,
        FragmentShader,
        putVertex,
        putFragment
) where

import Data.Typeable

import FWGL.Shader.Language
import FWGL.Shader.Monad

type VertexShader g i o = Shader g i (VertexShaderOutput ': o) ()
type FragmentShader g i = Shader g i (FragmentShaderOutput ': '[]) ()

newtype VertexShaderOutput = VSO V4 deriving (Typeable, ShaderType)
newtype FragmentShaderOutput = FSO V4 deriving (Typeable, ShaderType)

putVertex :: Member VertexShaderOutput o => V4 -> Shader g i o ()
-- putVertex :: V4 -> Shader '[] '[] (VertexShaderOutput ': '[]) ()
putVertex = put . VSO

putFragment :: Member FragmentShaderOutput o => V4 -> Shader g i o ()
-- putFragment :: V4 -> Shader '[] '[] (FragmentShaderOutput ': '[]) ()
putFragment = put . FSO
