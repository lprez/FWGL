{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module FWGL.Internal.GL (
        GL,
        ActiveTexture(..),
        module FWGL.Backend.GLES,
        liftIO,
        evalGL,
        getCtx,
        activeTexture,
        attachShader,
        bindAttribLocation,
        bindBuffer,
        bindFramebuffer,
        bindRenderbuffer,
        bindTexture,
        blendColor,
        blendEquation,
        blendEquationSeparate,
        blendFunc,
        blendFuncSeparate,
        bufferData,
        bufferSubData,
        checkFramebufferStatus,
        clear,
        clearColor,
        clearDepth,
        clearStencil,
        colorMask,
        compileShader,
        compressedTexImage2D,
        compressedTexSubImage2D,
        copyTexImage2D,
        copyTexSubImage2D,
        createBuffer,
        createFramebuffer,
        createProgram,
        createRenderbuffer,
        createShader,
        createTexture,
        cullFace,
        deleteBuffer,
        deleteFramebuffer,
        deleteProgram,
        deleteRenderbuffer,
        deleteShader,
        deleteTexture,
        depthFunc,
        depthMask,
        depthRange,
        detachShader,
        disable,
        disableVertexAttribArray,
        drawArrays,
        drawElements,
        enable,
        enableVertexAttribArray,
        finish,
        flush,
        framebufferRenderbuffer,
        framebufferTexture2D,
        frontFace,
        generateMipmap,
        getActiveAttrib,
        getActiveUniform,
        getAttribLocation,
        getError,
        getFramebufferAttachmentParameter,
        getProgramInfoLog,
        getShaderPrecisionFormat,
        getShaderInfoLog,
        getShaderSource,
        getUniformLocation,
        getVertexAttribOffset,
        hint,
        isBuffer,
        isEnabled,
        isFramebuffer,
        isProgram,
        isRenderbuffer,
        isShader,
        isTexture,
        lineWidth,
        linkProgram,
        pixelStorei,
        polygonOffset,
        readPixels,
        renderbufferStorage,
        sampleCoverage,
        scissor,
        shaderSource,
        stencilFunc,
        stencilFuncSeparate,
        stencilMask,
        stencilMaskSeparate,
        stencilOp,
        stencilOpSeparate,
        texImage2DBuffer,
        texImage2DImage,
        texParameterf,
        texParameteri,
        texSubImage2D,
        uniform1f,
        uniform1fv,
        uniform1i,
        uniform1iv,
        uniform2f,
        uniform2fv,
        uniform2i,
        uniform2iv,
        uniform3f,
        uniform3fv,
        uniform3i,
        uniform3iv,
        uniform4f,
        uniform4fv,
        uniform4i,
        uniform4iv,
        uniformMatrix2fv,
        uniformMatrix3fv,
        uniformMatrix4fv,
        useProgram,
        validateProgram,
        vertexAttrib1f,
        vertexAttrib1fv,
        vertexAttrib2f,
        vertexAttrib2fv,
        vertexAttrib3f,
        vertexAttrib3fv,
        vertexAttrib4f,
        vertexAttrib4fv,
        vertexAttribPointer,
        viewport
) where

import Control.Applicative
import Control.Monad.IO.Class
import Control.Monad.Trans.State
import Data.Word
import FWGL.Backend.GLES
        
-- TODO: ReaderT, not StateT
newtype GL a = GL (StateT Ctx IO a)
        deriving (Functor, Applicative, Monad, MonadIO)

newtype ActiveTexture = ActiveTexture Word

evalGL :: GL a -> Ctx -> IO a
evalGL (GL m) = evalStateT m

getCtx :: GLES => GL Ctx
getCtx = GL get

activeTexture :: GLES => Word -> GL ()
activeTexture a = GL get >>= \ctx -> liftIO $ glActiveTexture ctx a

attachShader :: GLES => Program -> Shader -> GL ()
attachShader a b = GL get >>= \ctx -> liftIO $ glAttachShader ctx a b

bindAttribLocation :: GLES => Program -> Word -> GLString -> GL ()
bindAttribLocation a b c = GL get >>= \ctx -> liftIO $ glBindAttribLocation ctx a b c

bindBuffer :: GLES => Word -> Buffer -> GL ()
bindBuffer a b = GL get >>= \ctx -> liftIO $ glBindBuffer ctx a b

bindFramebuffer :: GLES => Word -> FrameBuffer -> GL ()
bindFramebuffer a b = GL get >>= \ctx -> liftIO $ glBindFramebuffer ctx a b

bindRenderbuffer :: GLES => Word -> RenderBuffer -> GL ()
bindRenderbuffer a b = GL get >>= \ctx -> liftIO $ glBindRenderbuffer ctx a b

bindTexture :: GLES => Word -> Texture -> GL ()
bindTexture a b = GL get >>= \ctx -> liftIO $ glBindTexture ctx a b

blendColor :: GLES => Float -> Float -> Float -> Float -> GL ()
blendColor a b c d = GL get >>= \ctx -> liftIO $ glBlendColor ctx a b c d

blendEquation :: GLES => Word -> GL ()
blendEquation a = GL get >>= \ctx -> liftIO $ glBlendEquation ctx a

blendEquationSeparate :: GLES => Word -> Word -> GL ()
blendEquationSeparate a b = GL get >>= \ctx -> liftIO $ glBlendEquationSeparate ctx a b

blendFunc :: GLES => Word -> Word -> GL ()
blendFunc a b = GL get >>= \ctx -> liftIO $ glBlendFunc ctx a b

blendFuncSeparate :: GLES => Word -> Word -> Word -> Word -> GL ()
blendFuncSeparate a b c d = GL get >>= \ctx -> liftIO $ glBlendFuncSeparate ctx a b c d

bufferData :: GLES => Word -> Array -> Word -> GL ()
bufferData a b c = GL get >>= \ctx -> liftIO $ glBufferData ctx a b c

bufferSubData :: GLES => Word -> Word -> Array -> GL ()
bufferSubData a b c = GL get >>= \ctx -> liftIO $ glBufferSubData ctx a b c

checkFramebufferStatus :: GLES => Word -> GL Word
checkFramebufferStatus a = GL get >>= \ctx -> liftIO $ glCheckFramebufferStatus ctx a

clear :: GLES => Word -> GL ()
clear a = GL get >>= \ctx -> liftIO $ glClear ctx a

clearColor :: GLES => Float -> Float -> Float -> Float -> GL ()
clearColor a b c d = GL get >>= \ctx -> liftIO $ glClearColor ctx a b c d

clearDepth :: GLES => Float -> GL ()
clearDepth a = GL get >>= \ctx -> liftIO $ glClearDepth ctx a

clearStencil :: GLES => Int -> GL ()
clearStencil a = GL get >>= \ctx -> liftIO $ glClearStencil ctx a

colorMask :: GLES => Bool -> Bool -> Bool -> Bool -> GL ()
colorMask a b c d = GL get >>= \ctx -> liftIO $ glColorMask ctx a b c d

compileShader :: GLES => Shader -> GL ()
compileShader a = GL get >>= \ctx -> liftIO $ glCompileShader ctx a

compressedTexImage2D :: GLES => Word -> Int -> Word -> Int -> Int -> Int -> Array -> GL ()
compressedTexImage2D a b c d e f g = GL get >>= \ctx -> liftIO $ glCompressedTexImage2D ctx a b c d e f g

compressedTexSubImage2D :: GLES => Word -> Int -> Int -> Int -> Int -> Int -> Word -> Array -> GL ()
compressedTexSubImage2D a b c d e f g h = GL get >>= \ctx -> liftIO $ glCompressedTexSubImage2D ctx a b c d e f g h

copyTexImage2D :: GLES => Word -> Int -> Word -> Int -> Int -> Int -> Int -> Int -> GL ()
copyTexImage2D a b c d e f g h = GL get >>= \ctx -> liftIO $ glCopyTexImage2D ctx a b c d e f g h

copyTexSubImage2D :: GLES => Word -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> GL ()
copyTexSubImage2D a b c d e f g h = GL get >>= \ctx -> liftIO $ glCopyTexSubImage2D ctx a b c d e f g h

createBuffer :: GLES => GL Buffer
createBuffer = GL get >>= liftIO . glCreateBuffer

createFramebuffer :: GLES => GL FrameBuffer
createFramebuffer = GL get >>= liftIO . glCreateFramebuffer

createProgram :: GLES => GL Program
createProgram = GL get >>= liftIO . glCreateProgram

createRenderbuffer :: GLES => GL RenderBuffer
createRenderbuffer = GL get >>= liftIO . glCreateRenderbuffer

createShader :: GLES => Word -> GL Shader
createShader a = GL get >>= \ctx -> liftIO $ glCreateShader ctx a

createTexture :: GLES => GL Texture
createTexture = GL get >>= liftIO . glCreateTexture

cullFace :: GLES => Word -> GL ()
cullFace a = GL get >>= \ctx -> liftIO $ glCullFace ctx a

deleteBuffer :: GLES => Buffer -> GL ()
deleteBuffer a = GL get >>= \ctx -> liftIO $ glDeleteBuffer ctx a

deleteFramebuffer :: GLES => FrameBuffer -> GL ()
deleteFramebuffer a = GL get >>= \ctx -> liftIO $ glDeleteFramebuffer ctx a

deleteProgram :: GLES => Program -> GL ()
deleteProgram a = GL get >>= \ctx -> liftIO $ glDeleteProgram ctx a

deleteRenderbuffer :: GLES => RenderBuffer -> GL ()
deleteRenderbuffer a = GL get >>= \ctx -> liftIO $ glDeleteRenderbuffer ctx a

deleteShader :: GLES => Shader -> GL ()
deleteShader a = GL get >>= \ctx -> liftIO $ glDeleteShader ctx a

deleteTexture :: GLES => Texture -> GL ()
deleteTexture a = GL get >>= \ctx -> liftIO $ glDeleteTexture ctx a

depthFunc :: GLES => Word -> GL ()
depthFunc a = GL get >>= \ctx -> liftIO $ glDepthFunc ctx a

depthMask :: GLES => Bool -> GL ()
depthMask a = GL get >>= \ctx -> liftIO $ glDepthMask ctx a

depthRange :: GLES => Float -> Float -> GL ()
depthRange a b = GL get >>= \ctx -> liftIO $ glDepthRange ctx a b

detachShader :: GLES => Program -> Shader -> GL ()
detachShader a b = GL get >>= \ctx -> liftIO $ glDetachShader ctx a b

disable :: GLES => Word -> GL ()
disable a = GL get >>= \ctx -> liftIO $ glDisable ctx a

disableVertexAttribArray :: GLES => Word -> GL ()
disableVertexAttribArray a = GL get >>= \ctx -> liftIO $ glDisableVertexAttribArray ctx a

drawArrays :: GLES => Word -> Int -> Int -> GL ()
drawArrays a b c = GL get >>= \ctx -> liftIO $ glDrawArrays ctx a b c

drawElements :: GLES => Word -> Int -> Word -> Word -> GL ()
drawElements a b c d = GL get >>= \ctx -> liftIO $ glDrawElements ctx a b c d

enable :: GLES => Word -> GL ()
enable a = GL get >>= \ctx -> liftIO $ glEnable ctx a

enableVertexAttribArray :: GLES => Word -> GL ()
enableVertexAttribArray a = GL get >>= \ctx -> liftIO $ glEnableVertexAttribArray ctx a

finish :: GLES => GL ()
finish = GL get >>= liftIO . glFinish

flush :: GLES => GL ()
flush = GL get >>= liftIO . glFlush

framebufferRenderbuffer :: GLES => Word -> Word -> Word -> RenderBuffer -> GL ()
framebufferRenderbuffer a b c d = GL get >>= \ctx -> liftIO $ glFramebufferRenderbuffer ctx a b c d

framebufferTexture2D :: GLES => Word -> Word -> Word -> Texture -> Int -> GL ()
framebufferTexture2D a b c d e = GL get >>= \ctx -> liftIO $ glFramebufferTexture2D ctx a b c d e

frontFace :: GLES => Word -> GL ()
frontFace a = GL get >>= \ctx -> liftIO $ glFrontFace ctx a

generateMipmap :: GLES => Word -> GL ()
generateMipmap a = GL get >>= \ctx -> liftIO $ glGenerateMipmap ctx a

getActiveAttrib :: GLES => Program -> Word -> GL ActiveInfo
getActiveAttrib a b = GL get >>= \ctx -> liftIO $ glGetActiveAttrib ctx a b

getActiveUniform :: GLES => Program -> Word -> GL ActiveInfo
getActiveUniform a b = GL get >>= \ctx -> liftIO $ glGetActiveUniform ctx a b

getAttribLocation :: GLES => Program -> GLString -> GL Int
getAttribLocation a b = GL get >>= \ctx -> liftIO $ glGetAttribLocation ctx a b

-- getBufferParameter :: GLES => Word -> Word -> GL (JSRef a)
-- getBufferParameter a b = GL get >>= \ctx -> liftIO $ glGetBufferParameter ctx a b

-- getParameter :: GLES => Word -> GL (JSRef a)
-- getParameter a = GL get >>= \ctx -> liftIO $ glGetParameter ctx a

getError :: GLES => GL Word
getError = GL get >>= liftIO . glGetError

getFramebufferAttachmentParameter :: GLES => Word -> Word -> GL Word
getFramebufferAttachmentParameter a b = GL get >>= \ctx -> liftIO $ glGetFramebufferAttachmentParameter ctx a b

getProgramInfoLog :: GLES => Program -> GL GLString
getProgramInfoLog a = GL get >>= \ctx -> liftIO $ glGetProgramInfoLog ctx a

-- getRenderbufferParameter :: GLES => Word -> Word -> GL (JSRef a)
-- getRenderbufferParameter a b = GL get >>= \ctx -> liftIO $ glGetRenderbufferParameter ctx a b

-- getShaderParameter :: GLES => Shader -> Word -> GL (JSRef a)
-- getShaderParameter a b = GL get >>= \ctx -> liftIO $ glGetShaderParameter ctx a b

getShaderPrecisionFormat :: GLES => Word -> Word -> GL ShaderPrecisionFormat
getShaderPrecisionFormat a b = GL get >>= \ctx -> liftIO $ glGetShaderPrecisionFormat ctx a b

getShaderInfoLog :: GLES => Shader -> GL GLString
getShaderInfoLog a = GL get >>= \ctx -> liftIO $ glGetShaderInfoLog ctx a

getShaderSource :: GLES => Shader -> GL GLString
getShaderSource a = GL get >>= \ctx -> liftIO $ glGetShaderSource ctx a

-- getTexParameter :: GLES => Word -> Word -> GL (JSRef a)
-- getTexParameter a b = GL get >>= \ctx -> liftIO $ glGetTexParameter ctx a b

-- getUniform :: GLES => Program -> UniformLocation -> GL (JSRef a)
-- getUniform a b = GL get >>= \ctx -> liftIO $ glGetUniform ctx a b

getUniformLocation :: GLES => Program -> GLString -> GL UniformLocation
getUniformLocation a b = GL get >>= \ctx -> liftIO $ glGetUniformLocation ctx a b

-- getVertexAttrib :: GLES => Word -> Word -> GL (JSRef a)
-- getVertexAttrib a b = GL get >>= \ctx -> liftIO $ glGetVertexAttrib ctx a b

getVertexAttribOffset :: GLES => Word -> Word -> GL Word
getVertexAttribOffset a b = GL get >>= \ctx -> liftIO $ glGetVertexAttribOffset ctx a b

hint :: GLES => Word -> Word -> GL ()
hint a b = GL get >>= \ctx -> liftIO $ glHint ctx a b

isBuffer :: GLES => Buffer -> GL Bool
isBuffer a = GL get >>= \ctx -> liftIO $ glIsBuffer ctx a

isEnabled :: GLES => Word -> GL Bool
isEnabled a = GL get >>= \ctx -> liftIO $ glIsEnabled ctx a

isFramebuffer :: GLES => FrameBuffer -> GL Bool
isFramebuffer a = GL get >>= \ctx -> liftIO $ glIsFramebuffer ctx a

isProgram :: GLES => Program -> GL Bool
isProgram a = GL get >>= \ctx -> liftIO $ glIsProgram ctx a

isRenderbuffer :: GLES => RenderBuffer -> GL Bool
isRenderbuffer a = GL get >>= \ctx -> liftIO $ glIsRenderbuffer ctx a

isShader :: GLES => Shader -> GL Bool
isShader a = GL get >>= \ctx -> liftIO $ glIsShader ctx a

isTexture :: GLES => Texture -> GL Bool
isTexture a = GL get >>= \ctx -> liftIO $ glIsTexture ctx a

lineWidth :: GLES => Float -> GL ()
lineWidth a = GL get >>= \ctx -> liftIO $ glLineWidth ctx a

linkProgram :: GLES => Program -> GL ()
linkProgram a = GL get >>= \ctx -> liftIO $ glLinkProgram ctx a

pixelStorei :: GLES => Word -> Int -> GL ()
pixelStorei a b = GL get >>= \ctx -> liftIO $ glPixelStorei ctx a b

polygonOffset :: GLES => Float -> Float -> GL ()
polygonOffset a b = GL get >>= \ctx -> liftIO $ glPolygonOffset ctx a b

readPixels :: GLES => Int -> Int -> Int -> Int -> Word -> Word -> Array -> GL ()
readPixels a b c d e f g = GL get >>= \ctx -> liftIO $ glReadPixels ctx a b c d e f g

renderbufferStorage :: GLES => Word -> Word -> Int -> Int -> GL ()
renderbufferStorage a b c d = GL get >>= \ctx -> liftIO $ glRenderbufferStorage ctx a b c d

sampleCoverage :: GLES => Float -> Bool -> GL ()
sampleCoverage a b = GL get >>= \ctx -> liftIO $ glSampleCoverage ctx a b

scissor :: GLES => Int -> Int -> Int -> Int -> GL ()
scissor a b c d = GL get >>= \ctx -> liftIO $ glScissor ctx a b c d

shaderSource :: GLES => Shader -> GLString -> GL ()
shaderSource a b = GL get >>= \ctx -> liftIO $ glShaderSource ctx a b

stencilFunc :: GLES => Word -> Int -> Word -> GL ()
stencilFunc a b c = GL get >>= \ctx -> liftIO $ glStencilFunc ctx a b c

stencilFuncSeparate :: GLES => Word -> Word -> Int -> Word -> GL ()
stencilFuncSeparate a b c d = GL get >>= \ctx -> liftIO $ glStencilFuncSeparate ctx a b c d

stencilMask :: GLES => Word -> GL ()
stencilMask a = GL get >>= \ctx -> liftIO $ glStencilMask ctx a

stencilMaskSeparate :: GLES => Word -> Word -> GL ()
stencilMaskSeparate a b = GL get >>= \ctx -> liftIO $ glStencilMaskSeparate ctx a b

stencilOp :: GLES => Word -> Word -> Word -> GL ()
stencilOp a b c = GL get >>= \ctx -> liftIO $ glStencilOp ctx a b c

stencilOpSeparate :: GLES => Word -> Word -> Word -> Word -> GL ()
stencilOpSeparate a b c d = GL get >>= \ctx -> liftIO $ glStencilOpSeparate ctx a b c d

texImage2DBuffer :: GLES => Word -> Int -> Word -> Int -> Int -> Int -> Word -> Word -> Array -> GL ()
texImage2DBuffer a b c d e f g h i = GL get >>= \ctx -> liftIO $ glTexImage2DBuffer ctx a b c d e f g h i

texImage2DImage :: GLES => Word -> Int -> Word -> Word -> Word -> Image -> GL ()
texImage2DImage a b c d e f = GL get >>= \ctx -> liftIO $ glTexImage2DImage ctx a b c d e f

texParameterf :: GLES => Word -> Word -> Float -> GL ()
texParameterf a b c = GL get >>= \ctx -> liftIO $ glTexParameterf ctx a b c

texParameteri :: GLES => Word -> Word -> Int -> GL ()
texParameteri a b c = GL get >>= \ctx -> liftIO $ glTexParameteri ctx a b c

texSubImage2D :: GLES => Word -> Int -> Int -> Int -> Int -> Int -> Word -> Word -> Array -> GL ()
texSubImage2D a b c d e f g h i = GL get >>= \ctx -> liftIO $ glTexSubImage2D ctx a b c d e f g h i

uniform1f :: GLES => UniformLocation -> Float -> GL ()
uniform1f a b = GL get >>= \ctx -> liftIO $ glUniform1f ctx a b

uniform1fv :: GLES => UniformLocation -> Float32Array -> GL ()
uniform1fv a b = GL get >>= \ctx -> liftIO $ glUniform1fv ctx a b

uniform1i :: GLES => UniformLocation -> Int -> GL ()
uniform1i a b = GL get >>= \ctx -> liftIO $ glUniform1i ctx a b

uniform1iv :: GLES => UniformLocation -> Int32Array -> GL ()
uniform1iv a b = GL get >>= \ctx -> liftIO $ glUniform1iv ctx a b

uniform2f :: GLES => UniformLocation -> Float -> Float -> GL ()
uniform2f a b c = GL get >>= \ctx -> liftIO $ glUniform2f ctx a b c

uniform2fv :: GLES => UniformLocation -> Float32Array -> GL ()
uniform2fv a b = GL get >>= \ctx -> liftIO $ glUniform2fv ctx a b

uniform2i :: GLES => UniformLocation -> Int -> Int -> GL ()
uniform2i a b c = GL get >>= \ctx -> liftIO $ glUniform2i ctx a b c

uniform2iv :: GLES => UniformLocation -> Int32Array -> GL ()
uniform2iv a b = GL get >>= \ctx -> liftIO $ glUniform2iv ctx a b

uniform3f :: GLES => UniformLocation -> Float -> Float -> Float -> GL ()
uniform3f a b c d = GL get >>= \ctx -> liftIO $ glUniform3f ctx a b c d

uniform3fv :: GLES => UniformLocation -> Float32Array -> GL ()
uniform3fv a b = GL get >>= \ctx -> liftIO $ glUniform3fv ctx a b

uniform3i :: GLES => UniformLocation -> Int -> Int -> Int -> GL ()
uniform3i a b c d = GL get >>= \ctx -> liftIO $ glUniform3i ctx a b c d

uniform3iv :: GLES => UniformLocation -> Int32Array -> GL ()
uniform3iv a b = GL get >>= \ctx -> liftIO $ glUniform3iv ctx a b

uniform4f :: GLES => UniformLocation -> Float -> Float -> Float -> Float -> GL ()
uniform4f a b c d e = GL get >>= \ctx -> liftIO $ glUniform4f ctx a b c d e

uniform4fv :: GLES => UniformLocation -> Float32Array -> GL ()
uniform4fv a b = GL get >>= \ctx -> liftIO $ glUniform4fv ctx a b

uniform4i :: GLES => UniformLocation -> Int -> Int -> Int -> Int -> GL ()
uniform4i a b c d e = GL get >>= \ctx -> liftIO $ glUniform4i ctx a b c d e

uniform4iv :: GLES => UniformLocation -> Int32Array -> GL ()
uniform4iv a b = GL get >>= \ctx -> liftIO $ glUniform4iv ctx a b

uniformMatrix2fv :: GLES => UniformLocation -> Bool -> Float32Array -> GL ()
uniformMatrix2fv a b c = GL get >>= \ctx -> liftIO $ glUniformMatrix2fv ctx a b c

uniformMatrix3fv :: GLES => UniformLocation -> Bool -> Float32Array -> GL ()
uniformMatrix3fv a b c = GL get >>= \ctx -> liftIO $ glUniformMatrix3fv ctx a b c

uniformMatrix4fv :: GLES => UniformLocation -> Bool -> Float32Array -> GL ()
uniformMatrix4fv a b c = GL get >>= \ctx -> liftIO $ glUniformMatrix4fv ctx a b c

useProgram :: GLES => Program -> GL ()
useProgram a = GL get >>= \ctx -> liftIO $ glUseProgram ctx a

validateProgram :: GLES => Program -> GL ()
validateProgram a = GL get >>= \ctx -> liftIO $ glValidateProgram ctx a

vertexAttrib1f :: GLES => Word -> Float -> GL ()
vertexAttrib1f a b = GL get >>= \ctx -> liftIO $ glVertexAttrib1f ctx a b

vertexAttrib1fv :: GLES => Word -> Float32Array -> GL ()
vertexAttrib1fv a b = GL get >>= \ctx -> liftIO $ glVertexAttrib1fv ctx a b

vertexAttrib2f :: GLES => Word -> Float -> Float -> GL ()
vertexAttrib2f a b c = GL get >>= \ctx -> liftIO $ glVertexAttrib2f ctx a b c

vertexAttrib2fv :: GLES => Word -> Float32Array -> GL ()
vertexAttrib2fv a b = GL get >>= \ctx -> liftIO $ glVertexAttrib2fv ctx a b

vertexAttrib3f :: GLES => Word -> Float -> Float -> Float -> GL ()
vertexAttrib3f a b c d = GL get >>= \ctx -> liftIO $ glVertexAttrib3f ctx a b c d

vertexAttrib3fv :: GLES => Word -> Float32Array -> GL ()
vertexAttrib3fv a b = GL get >>= \ctx -> liftIO $ glVertexAttrib3fv ctx a b

vertexAttrib4f :: GLES => Word -> Float -> Float -> Float -> Float -> GL ()
vertexAttrib4f a b c d e = GL get >>= \ctx -> liftIO $ glVertexAttrib4f ctx a b c d e

vertexAttrib4fv :: GLES => Word -> Float32Array -> GL ()
vertexAttrib4fv a b = GL get >>= \ctx -> liftIO $ glVertexAttrib4fv ctx a b

vertexAttribPointer :: GLES => Word -> Int -> Word -> Bool -> Int -> Word -> GL ()
vertexAttribPointer a b c d e f = GL get >>= \ctx -> liftIO $ glVertexAttribPointer ctx a b c d e f

viewport :: GLES => Int -> Int -> Int -> Int -> GL ()
viewport a b c d = GL get >>= \ctx -> liftIO $ glViewport ctx a b c d
