{-# LANGUAGE TypeFamilies, MultiParamTypeClasses, FunctionalDependencies #-}

-- FWGL.Shader.Variables? (+ loadUniform, loadAttribute, inputName, etc.)
module FWGL.Shader.CPU where

import Data.Word (Word)
import Data.Typeable
import qualified FWGL.Shader.Language as GPU
import FWGL.Internal.GL as CPU
import qualified FWGL.Vector as CPU
import Prelude as CPU

-- | CPU types convertible to GPU types (as uniforms).
class Typeable g => UniformCPU c g | g -> c where
        setUniform :: UniformLocation -> g -> c -> GL ()

-- | CPU types convertible to GPU types (as attributes).
class Typeable g => AttributeCPU c g | g -> c where
        encodeAttribute :: g -> [c] -> GL Array
        setAttribute :: g -> GLUInt -> GL ()

instance GLES => UniformCPU CPU.Float GPU.Float where
        setUniform l _ v = uniform1f l v

instance GLES => AttributeCPU CPU.Float GPU.Float where
        encodeAttribute _ a = liftIO $ encodeFloats a
        setAttribute _ i = attr i 1

-- TODO
instance GLES => UniformCPU CPU.ActiveTexture GPU.Sampler2D where
        setUniform l _ (CPU.ActiveTexture v) = uniform1i l $ fromIntegral v

instance GLES => UniformCPU CPU.V2 GPU.V2 where
        setUniform l _ (CPU.V2 x y) = uniform2f l x y

instance GLES => AttributeCPU CPU.V2 GPU.V2 where
        encodeAttribute _ a = liftIO $ encodeV2s a
        setAttribute _ i = attr i 2

instance GLES => UniformCPU CPU.V3 GPU.V3 where
        setUniform l _ (CPU.V3 x y z) = uniform3f l x y z

instance GLES => AttributeCPU CPU.V3 GPU.V3 where
        encodeAttribute _ a = liftIO $ encodeV3s a
        setAttribute _ i = attr i 3

instance GLES => UniformCPU CPU.V4 GPU.V4 where
        setUniform l _ (CPU.V4 x y z w) = uniform4f l x y z w

instance GLES => AttributeCPU CPU.V4 GPU.V4 where
        encodeAttribute _ a = liftIO $ encodeV4s a
        setAttribute _ i = attr i 4

instance GLES => UniformCPU CPU.M2 GPU.M2 where
        setUniform l _ m = liftIO (encodeM2 m) >>= uniformMatrix2fv l false

instance GLES => UniformCPU CPU.M3 GPU.M3 where
        setUniform l _ m = liftIO (encodeM3 m) >>= uniformMatrix3fv l false

instance GLES => UniformCPU CPU.M4 GPU.M4 where
        setUniform l _ m = liftIO (encodeM4 m) >>= uniformMatrix4fv l false

attr :: GLES => GLUInt -> GLInt -> GL ()
attr i s = vertexAttribPointer i s gl_FLOAT false 0 nullGLPtr
