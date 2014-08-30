module JavaScript.WebGL.Raw where

import Data.Int
import Data.Word

import GHCJS.Types

import JavaScript.WebGL.Types

foreign import javascript unsafe "$1.activeTexture($2)"
        activeTexture :: Ctx -> Word -> IO ()

foreign import javascript unsafe "$1.attachShader($2, $3)"
        attachShader :: Ctx -> Program -> Shader -> IO ()

foreign import javascript unsafe "$1.bindAttribLocation($2, $3, $4)"
        bindAttribLocation :: Ctx -> Program -> Word -> JSString -> IO ()

foreign import javascript unsafe "$1.bindBuffer($2, $3)"
        bindBuffer :: Ctx -> Word -> Buffer -> IO ()

foreign import javascript unsafe "$1.bindFramebuffer($2, $3)"
        bindFramebuffer :: Ctx -> Word -> FrameBuffer -> IO ()

foreign import javascript unsafe "$1.bindRenderbuffer($2, $3)"
        bindRenderbuffer :: Ctx -> Word -> RenderBuffer -> IO ()

foreign import javascript unsafe "$1.bindTexture($2, $3)"
        bindTexture :: Ctx -> Word -> Texture -> IO ()

foreign import javascript unsafe "$1.blendColor($2, $3, $4, $5)"
        blendColor :: Ctx -> Float -> Float -> Float -> Float -> IO ()

foreign import javascript unsafe "$1.blendEquation($2)"
        blendEquation :: Ctx -> Word -> IO ()

foreign import javascript unsafe "$1.blendEquationSeparate($2, $3)"
        blendEquationSeparate :: Ctx -> Word -> Word -> IO ()

foreign import javascript unsafe "$1.blendFunc($2, $3)"
        blendFunc :: Ctx -> Word -> Word -> IO ()

foreign import javascript unsafe "$1.blendFuncSeparate($2, $3, $4, $5)"
        blendFuncSeparate :: Ctx -> Word -> Word -> Word -> Word -> IO ()

{-
foreign import javascript unsafe "$1.bufferData($2, $3, $4)"
        bufferData :: Ctx -> Word -> Word -> Word -> IO ()
-}

foreign import javascript unsafe "$1.bufferData($2, $3, $4)"
        bufferData :: Ctx -> Word -> ArrayBufferView -> Word -> IO ()

{-
foreign import javascript unsafe "$1.bufferData($2, $3, $4)"
        bufferData :: Ctx -> Word -> ArrayBuffer -> Word -> IO ()
-}

foreign import javascript unsafe "$1.bufferSubData($2, $3, $4)"
        bufferSubData :: Ctx -> Word -> Word -> ArrayBufferView -> IO ()

{-
foreign import javascript unsafe "$1.bufferSubData($2, $3, $4)"
        bufferSubData :: Ctx -> Word -> Word -> ArrayBuffer -> IO ()
-}

foreign import javascript unsafe "$1.checkFramebufferStatus($2)"
        checkFramebufferStatus :: Ctx -> Word -> IO Word

foreign import javascript unsafe "$1.clear($2)"
        clear :: Ctx -> Word -> IO ()

foreign import javascript unsafe "$1.clearColor($2, $3, $4, $5)"
        clearColor :: Ctx -> Float -> Float -> Float -> Float -> IO ()

foreign import javascript unsafe "$1.clearDepth($2)"
        clearDepth :: Ctx -> Float -> IO ()

foreign import javascript unsafe "$1.clearStencil($2)"
        clearStencil :: Ctx -> Int -> IO ()

foreign import javascript unsafe "$1.colorMask($2, $3, $4, $5)"
        colorMask :: Ctx -> Bool -> Bool -> Bool -> Bool -> IO ()

foreign import javascript unsafe "$1.compileShader($2)"
        compileShader :: Ctx -> Shader -> IO ()

foreign import javascript unsafe "$1.compressedTexImage2D($2, $3, $4, $5, $6, $7, $8)"
        compressedTexImage2D :: Ctx -> Word -> Int -> Word -> Int -> Int -> Int -> ArrayBufferView -> IO ()

foreign import javascript unsafe "$1.compressedTexSubImage2D($2, $3, $4, $5, $6, $7, $8, $9)"
        compressedTexSubImage2D :: Ctx -> Word -> Int -> Int -> Int -> Int -> Int -> Word -> ArrayBufferView -> IO ()

foreign import javascript unsafe "$1.copyTexImage2D($2, $3, $4, $5, $6, $7, $8, $9)"
        copyTexImage2D :: Ctx -> Word -> Int -> Word -> Int -> Int -> Int -> Int -> Int -> IO ()

foreign import javascript unsafe "$1.copyTexSubImage2D($2, $3, $4, $5, $6, $7, $8, $9)"
        copyTexSubImage2D :: Ctx -> Word -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> IO ()

foreign import javascript unsafe "$1.createBuffer()"
        createBuffer :: Ctx -> IO Buffer

foreign import javascript unsafe "$1.createFramebuffer()"
        createFramebuffer :: Ctx -> IO FrameBuffer

foreign import javascript unsafe "$1.createProgram()"
        createProgram :: Ctx -> IO Program

foreign import javascript unsafe "$1.createRenderbuffer()"
        createRenderbuffer :: Ctx -> IO RenderBuffer

foreign import javascript unsafe "$1.createShader($2)"
        createShader :: Ctx -> Word -> IO Shader

foreign import javascript unsafe "$1.createTexture()"
        createTexture :: Ctx -> IO Texture

foreign import javascript unsafe "$1.cullFace($2)"
        cullFace :: Ctx -> Word -> IO ()

foreign import javascript unsafe "$1.deleteBuffer($2)"
        deleteBuffer :: Ctx -> Buffer -> IO ()

foreign import javascript unsafe "$1.deleteFramebuffer($2)"
        deleteFramebuffer :: Ctx -> FrameBuffer -> IO ()

foreign import javascript unsafe "$1.deleteProgram($2)"
        deleteProgram :: Ctx -> Program -> IO ()

foreign import javascript unsafe "$1.deleteRenderbuffer($2)"
        deleteRenderbuffer :: Ctx -> RenderBuffer -> IO ()

foreign import javascript unsafe "$1.deleteShader($2)"
        deleteShader :: Ctx -> Shader -> IO ()

foreign import javascript unsafe "$1.deleteTexture($2)"
        deleteTexture :: Ctx -> Texture -> IO ()

foreign import javascript unsafe "$1.depthFunc($2)"
        depthFunc :: Ctx -> Word -> IO ()

foreign import javascript unsafe "$1.depthMask($2)"
        depthMask :: Ctx -> Bool -> IO ()

foreign import javascript unsafe "$1.depthRange($2, $3)"
        depthRange :: Ctx -> Float -> Float -> IO ()

foreign import javascript unsafe "$1.detachShader($2, $3)"
        detachShader :: Ctx -> Program -> Shader -> IO ()

foreign import javascript unsafe "$1.disable($2)"
        disable :: Ctx -> Word -> IO ()

foreign import javascript unsafe "$1.disableVertexAttribArray($2)"
        disableVertexAttribArray :: Ctx -> Word -> IO ()

foreign import javascript unsafe "$1.drawArrays($2, $3, $4)"
        drawArrays :: Ctx -> Word -> Int -> Int -> IO ()

foreign import javascript unsafe "$1.drawElements($2, $3, $4, $5)"
        drawElements :: Ctx -> Word -> Int -> Word -> Word -> IO ()

foreign import javascript unsafe "$1.enable($2)"
        enable :: Ctx -> Word -> IO ()

foreign import javascript unsafe "$1.enableVertexAttribArray($2)"
        enableVertexAttribArray :: Ctx -> Word -> IO ()

foreign import javascript unsafe "$1.finish()"
        finish :: Ctx -> IO ()

foreign import javascript unsafe "$1.flush()"
        flush :: Ctx -> IO ()

foreign import javascript unsafe "$1.framebufferRenderbuffer($2, $3, $4, $5)"
        framebufferRenderbuffer :: Ctx -> Word -> Word -> Word -> RenderBuffer -> IO ()

foreign import javascript unsafe "$1.framebufferTexture2D($2, $3, $4, $5, $6)"
        framebufferTexture2D :: Ctx -> Word -> Word -> Word -> Texture -> Int -> IO ()

foreign import javascript unsafe "$1.frontFace($2)"
        frontFace :: Ctx -> Word -> IO ()

foreign import javascript unsafe "$1.generateMipmap($2)"
        generateMipmap :: Ctx -> Word -> IO ()

foreign import javascript unsafe "$1.getActiveAttrib($2, $3)"
        getActiveAttrib :: Ctx -> Program -> Word -> IO ActiveInfo

foreign import javascript unsafe "$1.getActiveUniform($2, $3)"
        getActiveUniform :: Ctx -> Program -> Word -> IO ActiveInfo

{-
foreign import javascript unsafe "$1.getAttachedShaders($2)"
        getAttachedShaders :: Ctx -> Program -> IO (Sequence Shader)
-}

foreign import javascript unsafe "$1.getAttribLocation($2, $3)"
        getAttribLocation :: Ctx -> Program -> JSString -> IO Int

foreign import javascript unsafe "$1.getBufferParameter($2, $3)"
        getBufferParameter :: Ctx -> Word -> Word -> IO (JSRef a)

foreign import javascript unsafe "$1.getParameter($2)"
        getParameter :: Ctx -> Word -> IO (JSRef a)

foreign import javascript unsafe "$1.getError()"
        getError :: Ctx -> IO Word

foreign import javascript unsafe "$1.getFramebufferAttachmentParameter($2, $3)"
        getFramebufferAttachmentParameter :: Ctx -> Word -> Word -> IO Word

foreign import javascript unsafe "$1.getProgramInfoLog($2)"
        getProgramInfoLog :: Ctx -> Program -> IO JSString

foreign import javascript unsafe "$1.getRenderbufferParameter($2, $3)"
        getRenderbufferParameter :: Ctx -> Word -> Word -> IO (JSRef a)

foreign import javascript unsafe "$1.getShaderParameter($2, $3)"
        getShaderParameter :: Ctx -> Shader -> Word -> IO (JSRef a)

foreign import javascript unsafe "$1.getShaderPrecisionFormat($2, $3)"
        getShaderPrecisionFormat :: Ctx -> Word -> Word -> IO ShaderPrecisionFormat

foreign import javascript unsafe "$1.getShaderInfoLog($2)"
        getShaderInfoLog :: Ctx -> Shader -> IO JSString

foreign import javascript unsafe "$1.getShaderSource($2)"
        getShaderSource :: Ctx -> Shader -> IO JSString

foreign import javascript unsafe "$1.getTexParameter($2, $3)"
        getTexParameter :: Ctx -> Word -> Word -> IO (JSRef a)

foreign import javascript unsafe "$1.getUniform($2, $3)"
        getUniform :: Ctx -> Program -> UniformLocation -> IO (JSRef a)

foreign import javascript unsafe "$1.getUniformLocation($2, $3)"
        getUniformLocation :: Ctx -> Program -> JSString -> IO UniformLocation

foreign import javascript unsafe "$1.getVertexAttrib($2, $3)"
        getVertexAttrib :: Ctx -> Word -> Word -> IO (JSRef a)

foreign import javascript unsafe "$1.getVertexAttribOffset($2, $3)"
        getVertexAttribOffset :: Ctx -> Word -> Word -> IO Word

foreign import javascript unsafe "$1.hint($2, $3)"
        hint :: Ctx -> Word -> Word -> IO ()

foreign import javascript unsafe "$1.isBuffer($2)"
        isBuffer :: Ctx -> Buffer -> IO Bool

foreign import javascript unsafe "$1.isEnabled($2)"
        isEnabled :: Ctx -> Word -> IO Bool

foreign import javascript unsafe "$1.isFramebuffer($2)"
        isFramebuffer :: Ctx -> FrameBuffer -> IO Bool

foreign import javascript unsafe "$1.isProgram($2)"
        isProgram :: Ctx -> Program -> IO Bool

foreign import javascript unsafe "$1.isRenderbuffer($2)"
        isRenderbuffer :: Ctx -> RenderBuffer -> IO Bool

foreign import javascript unsafe "$1.isShader($2)"
        isShader :: Ctx -> Shader -> IO Bool

foreign import javascript unsafe "$1.isTexture($2)"
        isTexture :: Ctx -> Texture -> IO Bool

foreign import javascript unsafe "$1.lineWidth($2)"
        lineWidth :: Ctx -> Float -> IO ()

foreign import javascript unsafe "$1.linkProgram($2)"
        linkProgram :: Ctx -> Program -> IO ()

foreign import javascript unsafe "$1.pixelStorei($2, $3)"
        pixelStorei :: Ctx -> Word -> Int -> IO ()

foreign import javascript unsafe "$1.polygonOffset($2, $3)"
        polygonOffset :: Ctx -> Float -> Float -> IO ()

foreign import javascript unsafe "$1.readPixels($2, $3, $4, $5, $6, $7, $8)"
        readPixels :: Ctx -> Int -> Int -> Int -> Int -> Word -> Word -> ArrayBufferView -> IO ()

foreign import javascript unsafe "$1.renderbufferStorage($2, $3, $4, $5)"
        renderbufferStorage :: Ctx -> Word -> Word -> Int -> Int -> IO ()

foreign import javascript unsafe "$1.sampleCoverage($2, $3)"
        sampleCoverage :: Ctx -> Float -> Bool -> IO ()

foreign import javascript unsafe "$1.scissor($2, $3, $4, $5)"
        scissor :: Ctx -> Int -> Int -> Int -> Int -> IO ()

foreign import javascript unsafe "$1.shaderSource($2, $3)"
        shaderSource :: Ctx -> Shader -> JSString -> IO ()

foreign import javascript unsafe "$1.stencilFunc($2, $3, $4)"
        stencilFunc :: Ctx -> Word -> Int -> Word -> IO ()

foreign import javascript unsafe "$1.stencilFuncSeparate($2, $3, $4, $5)"
        stencilFuncSeparate :: Ctx -> Word -> Word -> Int -> Word -> IO ()

foreign import javascript unsafe "$1.stencilMask($2)"
        stencilMask :: Ctx -> Word -> IO ()

foreign import javascript unsafe "$1.stencilMaskSeparate($2, $3)"
        stencilMaskSeparate :: Ctx -> Word -> Word -> IO ()

foreign import javascript unsafe "$1.stencilOp($2, $3, $4)"
        stencilOp :: Ctx -> Word -> Word -> Word -> IO ()

foreign import javascript unsafe "$1.stencilOpSeparate($2, $3, $4, $5)"
        stencilOpSeparate :: Ctx -> Word -> Word -> Word -> Word -> IO ()

foreign import javascript unsafe "$1.texImage2D($2, $3, $4, $5, $6, $7, $8, $9, $10)"
        texImage2D :: Ctx -> Word -> Int -> Word -> Int -> Int -> Int -> Word -> Word -> ArrayBufferView -> IO ()

{-
foreign import javascript unsafe "$1.texImage2D()"
        texImage2D :: Ctx -> Word -> Int -> Word -> Word -> Word -> CanvasElement -> Void 

foreign import javascript unsafe "$1.texImage2D()"
        texImage2D :: Ctx -> Word -> Int -> Word -> Word -> Word -> VideoElement -> Void 
-}

foreign import javascript unsafe "$1.texParameterf($2, $3, $4)"
        texParameterf :: Ctx -> Word -> Word -> Float -> IO ()

foreign import javascript unsafe "$1.texParameteri($2, $3, $4)"
        texParameteri :: Ctx -> Word -> Word -> Int -> IO ()

foreign import javascript unsafe "$1.texSubImage2D($2, $3, $4, $5, $6, $7, $8, $9, $10)"
        texSubImage2D :: Ctx -> Word -> Int -> Int -> Int -> Int -> Int -> Word -> Word -> ArrayBufferView -> IO ()

{-

foreign import javascript unsafe "$1.texSubImage2D()"
        texSubImage2D :: Ctx -> Word -> Int -> Int -> Int -> Word -> Word -> CanvasElement -> Void 

foreign import javascript unsafe "$1.texSubImage2D()"
        texSubImage2D :: Ctx -> Word -> Int -> Int -> Int -> Word -> Word -> VideoElement -> Void 

-}

foreign import javascript unsafe "$1.uniform1f($2, $3)"
        uniform1f :: Ctx -> UniformLocation -> Float -> IO ()

foreign import javascript unsafe "$1.uniform1fv($2, $3)"
        uniform1fv :: Ctx -> UniformLocation -> Float32Array -> IO ()

{-
foreign import javascript unsafe "$1.uniform1fv($2 Sequence Float ->)"
        uniform1fv :: Ctx -> UniformLocation -> Sequence Float -> IO ()
-}

foreign import javascript unsafe "$1.uniform1i($2, $3)"
        uniform1i :: Ctx -> UniformLocation -> Int -> IO ()

foreign import javascript unsafe "$1.uniform1iv($2, $3)"
        uniform1iv :: Ctx -> UniformLocation -> Int32Array -> IO ()

{-
foreign import javascript unsafe "$1.uniform1iv($2 Sequence Int ->)"
        uniform1iv :: Ctx -> UniformLocation -> Sequence Int -> IO ()
-}

foreign import javascript unsafe "$1.uniform2f($2, $3, $4)"
        uniform2f :: Ctx -> UniformLocation -> Float -> Float -> IO ()

foreign import javascript unsafe "$1.uniform2fv($2, $3)"
        uniform2fv :: Ctx -> UniformLocation -> Float32Array -> IO ()

{-
foreign import javascript unsafe "$1.uniform2fv($2 Sequence Float ->)"
        uniform2fv :: Ctx -> UniformLocation -> Sequence Float -> IO ()
-}

foreign import javascript unsafe "$1.uniform2i($2, $3, $4)"
        uniform2i :: Ctx -> UniformLocation -> Int -> Int -> IO ()

foreign import javascript unsafe "$1.uniform2iv($2, $3)"
        uniform2iv :: Ctx -> UniformLocation -> Int32Array -> IO ()

{-
foreign import javascript unsafe "$1.uniform2iv($2 Sequence Int ->)"
        uniform2iv :: Ctx -> UniformLocation -> Sequence Int -> IO ()
-}

foreign import javascript unsafe "$1.uniform3f($2, $3, $4, $5)"
        uniform3f :: Ctx -> UniformLocation -> Float -> Float -> Float -> IO ()

foreign import javascript unsafe "$1.uniform3fv($2, $3)"
        uniform3fv :: Ctx -> UniformLocation -> Float32Array -> IO ()

{-
foreign import javascript unsafe "$1.uniform3fv($2 Sequence Float ->)"
        uniform3fv :: Ctx -> UniformLocation -> Sequence Float -> IO ()
-}

foreign import javascript unsafe "$1.uniform3i($2, $3, $4, $5)"
        uniform3i :: Ctx -> UniformLocation -> Int -> Int -> Int -> IO ()

foreign import javascript unsafe "$1.uniform3iv($2, $3)"
        uniform3iv :: Ctx -> UniformLocation -> Int32Array -> IO ()

{-
foreign import javascript unsafe "$1.uniform3iv($2 Sequence Int ->)"
        uniform3iv :: Ctx -> UniformLocation -> Sequence Int -> IO ()
-}

foreign import javascript unsafe "$1.uniform4f($2, $3, $4, $5, $6)"
        uniform4f :: Ctx -> UniformLocation -> Float -> Float -> Float -> Float -> IO ()

foreign import javascript unsafe "$1.uniform4fv($2, $3)"
        uniform4fv :: Ctx -> UniformLocation -> Float32Array -> IO ()

{-
foreign import javascript unsafe "$1.uniform4fv($2 Sequence Float ->)"
        uniform4fv :: Ctx -> UniformLocation -> Sequence Float -> IO ()
-}

foreign import javascript unsafe "$1.uniform4i($2, $3, $4, $5, $6)"
        uniform4i :: Ctx -> UniformLocation -> Int -> Int -> Int -> Int -> IO ()

foreign import javascript unsafe "$1.uniform4iv($2, $3)"
        uniform4iv :: Ctx -> UniformLocation -> Int32Array -> IO ()

{-
foreign import javascript unsafe "$1.uniform4iv($2 Sequence Int ->)"
        uniform4iv :: Ctx -> UniformLocation -> Sequence Int -> IO ()
-}

foreign import javascript unsafe "$1.uniformMatrix2fv($2, $3, $4)"
        uniformMatrix2fv :: Ctx -> UniformLocation -> Bool -> Float32Array -> IO ()

{-
foreign import javascript unsafe "$1.uniformMatrix2fv($2, $3 Sequence Float ->)"
        uniformMatrix2fv :: Ctx -> UniformLocation -> Bool -> Sequence Float -> IO ()
-}

foreign import javascript unsafe "$1.uniformMatrix3fv($2, $3, $4)"
        uniformMatrix3fv :: Ctx -> UniformLocation -> Bool -> Float32Array -> IO ()

{-
foreign import javascript unsafe "$1.uniformMatrix3fv($2, $3 Sequence Float ->)"
        uniformMatrix3fv :: Ctx -> UniformLocation -> Bool -> Sequence Float -> IO ()
-}

foreign import javascript unsafe "$1.uniformMatrix4fv($2, $3, $4)"
        uniformMatrix4fv :: Ctx -> UniformLocation -> Bool -> Float32Array -> IO ()

{-
foreign import javascript unsafe "$1.uniformMatrix4fv($2, $3 Sequence Float ->)"
        uniformMatrix4fv :: Ctx -> UniformLocation -> Bool -> Sequence Float -> IO ()
-}

foreign import javascript unsafe "$1.useProgram($2)"
        useProgram :: Ctx -> Program -> IO ()

foreign import javascript unsafe "$1.validateProgram($2)"
        validateProgram :: Ctx -> Program -> IO ()

foreign import javascript unsafe "$1.vertexAttrib1f($2, $3)"
        vertexAttrib1f :: Ctx -> Word -> Float -> IO ()

foreign import javascript unsafe "$1.vertexAttrib1fv($2, $3)"
        vertexAttrib1fv :: Ctx -> Word -> Float32Array -> IO ()

{-
foreign import javascript unsafe "$1.vertexAttrib1fv($2 Sequence Float ->)"
        vertexAttrib1fv :: Ctx -> Word -> Sequence Float -> IO ()
-}

foreign import javascript unsafe "$1.vertexAttrib2f($2, $3, $4)"
        vertexAttrib2f :: Ctx -> Word -> Float -> Float -> IO ()

foreign import javascript unsafe "$1.vertexAttrib2fv($2, $3)"
        vertexAttrib2fv :: Ctx -> Word -> Float32Array -> IO ()

{-
foreign import javascript unsafe "$1.vertexAttrib2fv($2 Sequence Float ->)"
        vertexAttrib2fv :: Ctx -> Word -> Sequence Float -> IO ()
-}

foreign import javascript unsafe "$1.vertexAttrib3f($2, $3, $4, $5)"
        vertexAttrib3f :: Ctx -> Word -> Float -> Float -> Float -> IO ()

foreign import javascript unsafe "$1.vertexAttrib3fv($2, $3)"
        vertexAttrib3fv :: Ctx -> Word -> Float32Array -> IO ()

{-
foreign import javascript unsafe "$1.vertexAttrib3fv($2 Sequence Float ->)"
        vertexAttrib3fv :: Ctx -> Word -> Sequence Float -> IO ()
-}

foreign import javascript unsafe "$1.vertexAttrib4f($2, $3, $4, $5, $6)"
        vertexAttrib4f :: Ctx -> Word -> Float -> Float -> Float -> Float -> IO ()

foreign import javascript unsafe "$1.vertexAttrib4fv($2, $3)"
        vertexAttrib4fv :: Ctx -> Word -> Float32Array -> IO ()

{-
foreign import javascript unsafe "$1.vertexAttrib4fv($2 Sequence Float ->)"
        vertexAttrib4fv :: Ctx -> Word -> Sequence Float -> IO ()
-}

foreign import javascript unsafe "$1.vertexAttribPointer($2, $3, $4, $5, $6, $7)"
        vertexAttribPointer :: Ctx -> Word -> Int -> Word -> Bool -> Int -> Word -> IO ()

foreign import javascript unsafe "$1.viewport($2, $3, $4, $5)"
        viewport :: Ctx -> Int -> Int -> Int -> Int -> IO ()
