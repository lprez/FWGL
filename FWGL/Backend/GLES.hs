{-# LANGUAGE NullaryTypeClasses, TypeFamilies, MultiParamTypeClasses #-}

module FWGL.Backend.GLES where

import Data.Word
import FWGL.Graphics.Color
import FWGL.Vector

class GLES where
        type Ctx
        type GLString
        type Buffer
        type UniformLocation
        type Texture
        type Shader
        type Program
        type FrameBuffer
        type RenderBuffer
        type ActiveInfo
        type ShaderPrecisionFormat
        type Array
        type Float32Array
        type Int32Array
        type Image

        toGLString :: String -> GLString
        noBuffer :: Buffer
        noTexture :: Texture
        encodeM2 :: M2 -> IO Float32Array
        encodeM3 :: M3 -> IO Float32Array
        encodeM4 :: M4 -> IO Float32Array
        encodeFloats :: [Float] -> IO Array
        encodeV2s :: [V2] -> IO Array
        encodeV3s :: [V3] -> IO Array
        encodeV4s :: [V4] -> IO Array
        encodeUShorts :: [Word16] -> IO Array
        encodeColors :: [Color] -> IO Array

        glActiveTexture :: Ctx -> Word -> IO ()
        glAttachShader :: Ctx -> Program -> Shader -> IO ()
        glBindAttribLocation :: Ctx -> Program -> Word -> GLString -> IO ()
        glBindBuffer :: Ctx -> Word -> Buffer -> IO ()
        glBindFramebuffer :: Ctx -> Word -> FrameBuffer -> IO ()
        glBindRenderbuffer :: Ctx -> Word -> RenderBuffer -> IO ()
        glBindTexture :: Ctx -> Word -> Texture -> IO ()
        glBlendColor :: Ctx -> Float -> Float -> Float -> Float -> IO ()
        glBlendEquation :: Ctx -> Word -> IO ()
        glBlendEquationSeparate :: Ctx -> Word -> Word -> IO ()
        glBlendFunc :: Ctx -> Word -> Word -> IO ()
        glBlendFuncSeparate :: Ctx -> Word -> Word -> Word -> Word -> IO ()
        glBufferData :: Ctx -> Word -> Array -> Word -> IO ()
        glBufferSubData :: Ctx -> Word -> Word -> Array -> IO ()
        glCheckFramebufferStatus :: Ctx -> Word -> IO Word
        glClear :: Ctx -> Word -> IO ()
        glClearColor :: Ctx -> Float -> Float -> Float -> Float -> IO ()
        glClearDepth :: Ctx -> Float -> IO ()
        glClearStencil :: Ctx -> Int -> IO ()
        glColorMask :: Ctx -> Bool -> Bool -> Bool -> Bool -> IO ()
        glCompileShader :: Ctx -> Shader -> IO ()
        glCompressedTexImage2D :: Ctx -> Word -> Int -> Word -> Int -> Int -> Int -> Array -> IO ()
        glCompressedTexSubImage2D :: Ctx -> Word -> Int -> Int -> Int -> Int -> Int -> Word -> Array -> IO ()
        glCopyTexImage2D :: Ctx -> Word -> Int -> Word -> Int -> Int -> Int -> Int -> Int -> IO ()
        glCopyTexSubImage2D :: Ctx -> Word -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> IO ()
        glCreateBuffer :: Ctx -> IO Buffer
        glCreateFramebuffer :: Ctx -> IO FrameBuffer
        glCreateProgram :: Ctx -> IO Program
        glCreateRenderbuffer :: Ctx -> IO RenderBuffer
        glCreateShader :: Ctx -> Word -> IO Shader
        glCreateTexture :: Ctx -> IO Texture
        glCullFace :: Ctx -> Word -> IO ()
        glDeleteBuffer :: Ctx -> Buffer -> IO ()
        glDeleteFramebuffer :: Ctx -> FrameBuffer -> IO ()
        glDeleteProgram :: Ctx -> Program -> IO ()
        glDeleteRenderbuffer :: Ctx -> RenderBuffer -> IO ()
        glDeleteShader :: Ctx -> Shader -> IO ()
        glDeleteTexture :: Ctx -> Texture -> IO ()
        glDepthFunc :: Ctx -> Word -> IO ()
        glDepthMask :: Ctx -> Bool -> IO ()
        glDepthRange :: Ctx -> Float -> Float -> IO ()
        glDetachShader :: Ctx -> Program -> Shader -> IO ()
        glDisable :: Ctx -> Word -> IO ()
        glDisableVertexAttribArray :: Ctx -> Word -> IO ()
        glDrawArrays :: Ctx -> Word -> Int -> Int -> IO ()
        glDrawElements :: Ctx -> Word -> Int -> Word -> Word -> IO ()
        glEnable :: Ctx -> Word -> IO ()
        glEnableVertexAttribArray :: Ctx -> Word -> IO ()
        glFinish :: Ctx -> IO ()
        glFlush :: Ctx -> IO ()
        glFramebufferRenderbuffer :: Ctx -> Word -> Word -> Word -> RenderBuffer -> IO ()
        glFramebufferTexture2D :: Ctx -> Word -> Word -> Word -> Texture -> Int -> IO ()
        glFrontFace :: Ctx -> Word -> IO ()
        glGenerateMipmap :: Ctx -> Word -> IO ()
        glGetActiveAttrib :: Ctx -> Program -> Word -> IO ActiveInfo
        glGetActiveUniform :: Ctx -> Program -> Word -> IO ActiveInfo
        glGetAttribLocation :: Ctx -> Program -> GLString -> IO Int
        -- glGetBufferParameter :: Ctx -> Word -> Word -> IO (JSRef a)
        -- glGetParameter :: Ctx -> Word -> IO (JSRef a)
        glGetError :: Ctx -> IO Word
        glGetFramebufferAttachmentParameter :: Ctx -> Word -> Word -> IO Word
        glGetProgramInfoLog :: Ctx -> Program -> IO GLString
        -- glGetRenderbufferParameter :: Ctx -> Word -> Word -> IO (JSRef a)
        -- glGetShaderParameter :: Ctx -> Shader -> Word -> IO (JSRef a)
        glGetShaderPrecisionFormat :: Ctx -> Word -> Word -> IO ShaderPrecisionFormat
        glGetShaderInfoLog :: Ctx -> Shader -> IO GLString
        glGetShaderSource :: Ctx -> Shader -> IO GLString
        -- glGetTexParameter :: Ctx -> Word -> Word -> IO (JSRef a)
        -- glGetUniform :: Ctx -> Program -> UniformLocation -> IO (JSRef a)
        glGetUniformLocation :: Ctx -> Program -> GLString -> IO UniformLocation
        -- glGetVertexAttrib :: Ctx -> Word -> Word -> IO (JSRef a)
        glGetVertexAttribOffset :: Ctx -> Word -> Word -> IO Word
        glHint :: Ctx -> Word -> Word -> IO ()
        glIsBuffer :: Ctx -> Buffer -> IO Bool
        glIsEnabled :: Ctx -> Word -> IO Bool
        glIsFramebuffer :: Ctx -> FrameBuffer -> IO Bool
        glIsProgram :: Ctx -> Program -> IO Bool
        glIsRenderbuffer :: Ctx -> RenderBuffer -> IO Bool
        glIsShader :: Ctx -> Shader -> IO Bool
        glIsTexture :: Ctx -> Texture -> IO Bool
        glLineWidth :: Ctx -> Float -> IO ()
        glLinkProgram :: Ctx -> Program -> IO ()
        glPixelStorei :: Ctx -> Word -> Int -> IO ()
        glPolygonOffset :: Ctx -> Float -> Float -> IO ()
        glReadPixels :: Ctx -> Int -> Int -> Int -> Int -> Word -> Word -> Array -> IO ()
        glRenderbufferStorage :: Ctx -> Word -> Word -> Int -> Int -> IO ()
        glSampleCoverage :: Ctx -> Float -> Bool -> IO ()
        glScissor :: Ctx -> Int -> Int -> Int -> Int -> IO ()
        glShaderSource :: Ctx -> Shader -> GLString -> IO ()
        glStencilFunc :: Ctx -> Word -> Int -> Word -> IO ()
        glStencilFuncSeparate :: Ctx -> Word -> Word -> Int -> Word -> IO ()
        glStencilMask :: Ctx -> Word -> IO ()
        glStencilMaskSeparate :: Ctx -> Word -> Word -> IO ()
        glStencilOp :: Ctx -> Word -> Word -> Word -> IO ()
        glStencilOpSeparate :: Ctx -> Word -> Word -> Word -> Word -> IO ()
        glTexImage2DBuffer :: Ctx -> Word -> Int -> Word -> Int -> Int -> Int -> Word -> Word -> Array -> IO ()
        glTexImage2DImage :: Ctx -> Word -> Int -> Word -> Word -> Word -> Image -> IO ()
        glTexParameterf :: Ctx -> Word -> Word -> Float -> IO ()
        glTexParameteri :: Ctx -> Word -> Word -> Int -> IO ()
        glTexSubImage2D :: Ctx -> Word -> Int -> Int -> Int -> Int -> Int -> Word -> Word -> Array -> IO ()
        glUniform1f :: Ctx -> UniformLocation -> Float -> IO ()
        glUniform1fv :: Ctx -> UniformLocation -> Float32Array -> IO ()
        glUniform1i :: Ctx -> UniformLocation -> Int -> IO ()
        glUniform1iv :: Ctx -> UniformLocation -> Int32Array -> IO ()
        glUniform2f :: Ctx -> UniformLocation -> Float -> Float -> IO ()
        glUniform2fv :: Ctx -> UniformLocation -> Float32Array -> IO ()
        glUniform2i :: Ctx -> UniformLocation -> Int -> Int -> IO ()
        glUniform2iv :: Ctx -> UniformLocation -> Int32Array -> IO ()
        glUniform3f :: Ctx -> UniformLocation -> Float -> Float -> Float -> IO ()
        glUniform3fv :: Ctx -> UniformLocation -> Float32Array -> IO ()
        glUniform3i :: Ctx -> UniformLocation -> Int -> Int -> Int -> IO ()
        glUniform3iv :: Ctx -> UniformLocation -> Int32Array -> IO ()
        glUniform4f :: Ctx -> UniformLocation -> Float -> Float -> Float -> Float -> IO ()
        glUniform4fv :: Ctx -> UniformLocation -> Float32Array -> IO ()
        glUniform4i :: Ctx -> UniformLocation -> Int -> Int -> Int -> Int -> IO ()
        glUniform4iv :: Ctx -> UniformLocation -> Int32Array -> IO ()
        glUniformMatrix2fv :: Ctx -> UniformLocation -> Bool -> Float32Array -> IO ()
        glUniformMatrix3fv :: Ctx -> UniformLocation -> Bool -> Float32Array -> IO ()
        glUniformMatrix4fv :: Ctx -> UniformLocation -> Bool -> Float32Array -> IO ()
        glUseProgram :: Ctx -> Program -> IO ()
        glValidateProgram :: Ctx -> Program -> IO ()
        glVertexAttrib1f :: Ctx -> Word -> Float -> IO ()
        glVertexAttrib1fv :: Ctx -> Word -> Float32Array -> IO ()
        glVertexAttrib2f :: Ctx -> Word -> Float -> Float -> IO ()
        glVertexAttrib2fv :: Ctx -> Word -> Float32Array -> IO ()
        glVertexAttrib3f :: Ctx -> Word -> Float -> Float -> Float -> IO ()
        glVertexAttrib3fv :: Ctx -> Word -> Float32Array -> IO ()
        glVertexAttrib4f :: Ctx -> Word -> Float -> Float -> Float -> Float -> IO ()
        glVertexAttrib4fv :: Ctx -> Word -> Float32Array -> IO ()
        glVertexAttribPointer :: Ctx -> Word -> Int -> Word -> Bool -> Int -> Word -> IO ()
        glViewport :: Ctx -> Int -> Int -> Int -> Int -> IO ()

        gl_DEPTH_BUFFER_BIT :: Num a => a
        gl_STENCIL_BUFFER_BIT :: Num a => a
        gl_COLOR_BUFFER_BIT :: Num a => a
        gl_POINTS :: Num a => a
        gl_LINES :: Num a => a
        gl_LINE_LOOP :: Num a => a
        gl_LINE_STRIP :: Num a => a
        gl_TRIANGLES :: Num a => a
        gl_TRIANGLE_STRIP :: Num a => a
        gl_TRIANGLE_FAN :: Num a => a
        gl_ZERO :: Num a => a
        gl_ONE :: Num a => a
        gl_SRC_COLOR :: Num a => a
        gl_ONE_MINUS_SRC_COLOR :: Num a => a
        gl_SRC_ALPHA :: Num a => a
        gl_ONE_MINUS_SRC_ALPHA :: Num a => a
        gl_DST_ALPHA :: Num a => a
        gl_ONE_MINUS_DST_ALPHA :: Num a => a
        gl_DST_COLOR :: Num a => a
        gl_ONE_MINUS_DST_COLOR :: Num a => a
        gl_SRC_ALPHA_SATURATE :: Num a => a
        gl_FUNC_ADD :: Num a => a
        gl_BLEND_EQUATION :: Num a => a
        gl_BLEND_EQUATION_RGB :: Num a => a
        gl_BLEND_EQUATION_ALPHA :: Num a => a
        gl_FUNC_SUBTRACT :: Num a => a
        gl_FUNC_REVERSE_SUBTRACT :: Num a => a
        gl_BLEND_DST_RGB :: Num a => a
        gl_BLEND_SRC_RGB :: Num a => a
        gl_BLEND_DST_ALPHA :: Num a => a
        gl_BLEND_SRC_ALPHA :: Num a => a
        gl_CONSTANT_COLOR :: Num a => a
        gl_ONE_MINUS_CONSTANT_COLOR :: Num a => a
        gl_CONSTANT_ALPHA :: Num a => a
        gl_ONE_MINUS_CONSTANT_ALPHA :: Num a => a
        gl_BLEND_COLOR :: Num a => a
        gl_ARRAY_BUFFER :: Num a => a
        gl_ELEMENT_ARRAY_BUFFER :: Num a => a
        gl_ARRAY_BUFFER_BINDING :: Num a => a
        gl_ELEMENT_ARRAY_BUFFER_BINDING :: Num a => a
        gl_STREAM_DRAW :: Num a => a
        gl_STATIC_DRAW :: Num a => a
        gl_DYNAMIC_DRAW :: Num a => a
        gl_BUFFER_SIZE :: Num a => a
        gl_BUFFER_USAGE :: Num a => a
        gl_CURRENT_VERTEX_ATTRIB :: Num a => a
        gl_FRONT :: Num a => a
        gl_BACK :: Num a => a
        gl_FRONT_AND_BACK :: Num a => a
        gl_CULL_FACE :: Num a => a
        gl_BLEND :: Num a => a
        gl_DITHER :: Num a => a
        gl_STENCIL_TEST :: Num a => a
        gl_DEPTH_TEST :: Num a => a
        gl_SCISSOR_TEST :: Num a => a
        gl_POLYGON_OFFSET_FILL :: Num a => a
        gl_SAMPLE_ALPHA_TO_COVERAGE :: Num a => a
        gl_SAMPLE_COVERAGE :: Num a => a
        gl_NO_ERROR :: Num a => a
        gl_INVALID_ENUM :: Num a => a
        gl_INVALID_VALUE :: Num a => a
        gl_INVALID_OPERATION :: Num a => a
        gl_OUT_OF_MEMORY :: Num a => a
        gl_CW :: Num a => a
        gl_CCW :: Num a => a
        gl_LINE_WIDTH :: Num a => a
        gl_ALIASED_POINT_SIZE_RANGE :: Num a => a
        gl_ALIASED_LINE_WIDTH_RANGE :: Num a => a
        gl_CULL_FACE_MODE :: Num a => a
        gl_FRONT_FACE :: Num a => a
        gl_DEPTH_RANGE :: Num a => a
        gl_DEPTH_WRITEMASK :: Num a => a
        gl_DEPTH_CLEAR_VALUE :: Num a => a
        gl_DEPTH_FUNC :: Num a => a
        gl_STENCIL_CLEAR_VALUE :: Num a => a
        gl_STENCIL_FUNC :: Num a => a
        gl_STENCIL_FAIL :: Num a => a
        gl_STENCIL_PASS_DEPTH_FAIL :: Num a => a
        gl_STENCIL_PASS_DEPTH_PASS :: Num a => a
        gl_STENCIL_REF :: Num a => a
        gl_STENCIL_VALUE_MASK :: Num a => a
        gl_STENCIL_WRITEMASK :: Num a => a
        gl_STENCIL_BACK_FUNC :: Num a => a
        gl_STENCIL_BACK_FAIL :: Num a => a
        gl_STENCIL_BACK_PASS_DEPTH_FAIL :: Num a => a
        gl_STENCIL_BACK_PASS_DEPTH_PASS :: Num a => a
        gl_STENCIL_BACK_REF :: Num a => a
        gl_STENCIL_BACK_VALUE_MASK :: Num a => a
        gl_STENCIL_BACK_WRITEMASK :: Num a => a
        gl_VIEWPORT :: Num a => a
        gl_SCISSOR_BOX :: Num a => a
        gl_COLOR_CLEAR_VALUE :: Num a => a
        gl_COLOR_WRITEMASK :: Num a => a
        gl_UNPACK_ALIGNMENT :: Num a => a
        gl_PACK_ALIGNMENT :: Num a => a
        gl_MAX_TEXTURE_SIZE :: Num a => a
        gl_MAX_VIEWPORT_DIMS :: Num a => a
        gl_SUBPIXEL_BITS :: Num a => a
        gl_RED_BITS :: Num a => a
        gl_GREEN_BITS :: Num a => a
        gl_BLUE_BITS :: Num a => a
        gl_ALPHA_BITS :: Num a => a
        gl_DEPTH_BITS :: Num a => a
        gl_STENCIL_BITS :: Num a => a
        gl_POLYGON_OFFSET_UNITS :: Num a => a
        gl_POLYGON_OFFSET_FACTOR :: Num a => a
        gl_TEXTURE_BINDING_2D :: Num a => a
        gl_SAMPLE_BUFFERS :: Num a => a
        gl_SAMPLES :: Num a => a
        gl_SAMPLE_COVERAGE_VALUE :: Num a => a
        gl_SAMPLE_COVERAGE_INVERT :: Num a => a
        gl_COMPRESSED_TEXTURE_FORMATS :: Num a => a
        gl_DONT_CARE :: Num a => a
        gl_FASTEST :: Num a => a
        gl_NICEST :: Num a => a
        gl_GENERATE_MIPMAP_HINT :: Num a => a
        gl_BYTE :: Num a => a
        gl_UNSIGNED_BYTE :: Num a => a
        gl_SHORT :: Num a => a
        gl_UNSIGNED_SHORT :: Num a => a
        gl_INT :: Num a => a
        gl_UNSIGNED_INT :: Num a => a
        gl_FLOAT :: Num a => a
        gl_DEPTH_COMPONENT :: Num a => a
        gl_ALPHA :: Num a => a
        gl_RGB :: Num a => a
        gl_RGBA :: Num a => a
        gl_LUMINANCE :: Num a => a
        gl_LUMINANCE_ALPHA :: Num a => a
        gl_UNSIGNED_SHORT_4_4_4_4 :: Num a => a
        gl_UNSIGNED_SHORT_5_5_5_1 :: Num a => a
        gl_UNSIGNED_SHORT_5_6_5 :: Num a => a
        gl_FRAGMENT_SHADER :: Num a => a
        gl_VERTEX_SHADER :: Num a => a
        gl_MAX_VERTEX_ATTRIBS :: Num a => a
        gl_MAX_VERTEX_UNIFORM_VECTORS :: Num a => a
        gl_MAX_VARYING_VECTORS :: Num a => a
        gl_MAX_COMBINED_TEXTURE_IMAGE_UNITS :: Num a => a
        gl_MAX_VERTEX_TEXTURE_IMAGE_UNITS :: Num a => a
        gl_MAX_TEXTURE_IMAGE_UNITS :: Num a => a
        gl_MAX_FRAGMENT_UNIFORM_VECTORS :: Num a => a
        gl_SHADER_TYPE :: Num a => a
        gl_DELETE_STATUS :: Num a => a
        gl_LINK_STATUS :: Num a => a
        gl_VALIDATE_STATUS :: Num a => a
        gl_ATTACHED_SHADERS :: Num a => a
        gl_ACTIVE_UNIFORMS :: Num a => a
        gl_ACTIVE_ATTRIBUTES :: Num a => a
        gl_SHADING_LANGUAGE_VERSION :: Num a => a
        gl_CURRENT_PROGRAM :: Num a => a
        gl_NEVER :: Num a => a
        gl_LESS :: Num a => a
        gl_EQUAL :: Num a => a
        gl_LEQUAL :: Num a => a
        gl_GREATER :: Num a => a
        gl_NOTEQUAL :: Num a => a
        gl_GEQUAL :: Num a => a
        gl_ALWAYS :: Num a => a
        gl_KEEP :: Num a => a
        gl_REPLACE :: Num a => a
        gl_INCR :: Num a => a
        gl_DECR :: Num a => a
        gl_INVERT :: Num a => a
        gl_INCR_WRAP :: Num a => a
        gl_DECR_WRAP :: Num a => a
        gl_VENDOR :: Num a => a
        gl_RENDERER :: Num a => a
        gl_VERSION :: Num a => a
        gl_NEAREST :: Num a => a
        gl_LINEAR :: Num a => a
        gl_NEAREST_MIPMAP_NEAREST :: Num a => a
        gl_LINEAR_MIPMAP_NEAREST :: Num a => a
        gl_NEAREST_MIPMAP_LINEAR :: Num a => a
        gl_LINEAR_MIPMAP_LINEAR :: Num a => a
        gl_TEXTURE_MAG_FILTER :: Num a => a
        gl_TEXTURE_MIN_FILTER :: Num a => a
        gl_TEXTURE_WRAP_S :: Num a => a
        gl_TEXTURE_WRAP_T :: Num a => a
        gl_TEXTURE_2D :: Num a => a
        gl_TEXTURE :: Num a => a
        gl_TEXTURE_CUBE_MAP :: Num a => a
        gl_TEXTURE_BINDING_CUBE_MAP :: Num a => a
        gl_TEXTURE_CUBE_MAP_POSITIVE_X :: Num a => a
        gl_TEXTURE_CUBE_MAP_NEGATIVE_X :: Num a => a
        gl_TEXTURE_CUBE_MAP_POSITIVE_Y :: Num a => a
        gl_TEXTURE_CUBE_MAP_NEGATIVE_Y :: Num a => a
        gl_TEXTURE_CUBE_MAP_POSITIVE_Z :: Num a => a
        gl_TEXTURE_CUBE_MAP_NEGATIVE_Z :: Num a => a
        gl_MAX_CUBE_MAP_TEXTURE_SIZE :: Num a => a
        gl_TEXTURE0 :: Num a => a
        gl_TEXTURE1 :: Num a => a
        gl_TEXTURE2 :: Num a => a
        gl_TEXTURE3 :: Num a => a
        gl_TEXTURE4 :: Num a => a
        gl_TEXTURE5 :: Num a => a
        gl_TEXTURE6 :: Num a => a
        gl_TEXTURE7 :: Num a => a
        gl_TEXTURE8 :: Num a => a
        gl_TEXTURE9 :: Num a => a
        gl_TEXTURE10 :: Num a => a
        gl_TEXTURE11 :: Num a => a
        gl_TEXTURE12 :: Num a => a
        gl_TEXTURE13 :: Num a => a
        gl_TEXTURE14 :: Num a => a
        gl_TEXTURE15 :: Num a => a
        gl_TEXTURE16 :: Num a => a
        gl_TEXTURE17 :: Num a => a
        gl_TEXTURE18 :: Num a => a
        gl_TEXTURE19 :: Num a => a
        gl_TEXTURE20 :: Num a => a
        gl_TEXTURE21 :: Num a => a
        gl_TEXTURE22 :: Num a => a
        gl_TEXTURE23 :: Num a => a
        gl_TEXTURE24 :: Num a => a
        gl_TEXTURE25 :: Num a => a
        gl_TEXTURE26 :: Num a => a
        gl_TEXTURE27 :: Num a => a
        gl_TEXTURE28 :: Num a => a
        gl_TEXTURE29 :: Num a => a
        gl_TEXTURE30 :: Num a => a
        gl_TEXTURE31 :: Num a => a
        gl_ACTIVE_TEXTURE :: Num a => a
        gl_REPEAT :: Num a => a
        gl_CLAMP_TO_EDGE :: Num a => a
        gl_MIRRORED_REPEAT :: Num a => a
        gl_FLOAT_VEC2 :: Num a => a
        gl_FLOAT_VEC3 :: Num a => a
        gl_FLOAT_VEC4 :: Num a => a
        gl_INT_VEC2 :: Num a => a
        gl_INT_VEC3 :: Num a => a
        gl_INT_VEC4 :: Num a => a
        gl_BOOL :: Num a => a
        gl_BOOL_VEC2 :: Num a => a
        gl_BOOL_VEC3 :: Num a => a
        gl_BOOL_VEC4 :: Num a => a
        gl_FLOAT_MAT2 :: Num a => a
        gl_FLOAT_MAT3 :: Num a => a
        gl_FLOAT_MAT4 :: Num a => a
        gl_SAMPLER_2D :: Num a => a
        gl_SAMPLER_CUBE :: Num a => a
        gl_VERTEX_ATTRIB_ARRAY_ENABLED :: Num a => a
        gl_VERTEX_ATTRIB_ARRAY_SIZE :: Num a => a
        gl_VERTEX_ATTRIB_ARRAY_STRIDE :: Num a => a
        gl_VERTEX_ATTRIB_ARRAY_TYPE :: Num a => a
        gl_VERTEX_ATTRIB_ARRAY_NORMALIZED :: Num a => a
        gl_VERTEX_ATTRIB_ARRAY_POINTER :: Num a => a
        gl_VERTEX_ATTRIB_ARRAY_BUFFER_BINDING :: Num a => a
        gl_COMPILE_STATUS :: Num a => a
        gl_LOW_FLOAT :: Num a => a
        gl_MEDIUM_FLOAT :: Num a => a
        gl_HIGH_FLOAT :: Num a => a
        gl_LOW_INT :: Num a => a
        gl_MEDIUM_INT :: Num a => a
        gl_HIGH_INT :: Num a => a
        gl_FRAMEBUFFER :: Num a => a
        gl_RENDERBUFFER :: Num a => a
        gl_RGBA4 :: Num a => a
        gl_RGB5_A1 :: Num a => a
        gl_RGB565 :: Num a => a
        gl_DEPTH_COMPONENT16 :: Num a => a
        gl_STENCIL_INDEX :: Num a => a
        gl_STENCIL_INDEX8 :: Num a => a
        gl_DEPTH_STENCIL :: Num a => a
        gl_RENDERBUFFER_WIDTH :: Num a => a
        gl_RENDERBUFFER_HEIGHT :: Num a => a
        gl_RENDERBUFFER_INTERNAL_FORMAT :: Num a => a
        gl_RENDERBUFFER_RED_SIZE :: Num a => a
        gl_RENDERBUFFER_GREEN_SIZE :: Num a => a
        gl_RENDERBUFFER_BLUE_SIZE :: Num a => a
        gl_RENDERBUFFER_ALPHA_SIZE :: Num a => a
        gl_RENDERBUFFER_DEPTH_SIZE :: Num a => a
        gl_RENDERBUFFER_STENCIL_SIZE :: Num a => a
        gl_FRAMEBUFFER_ATTACHMENT_OBJECT_TYPE :: Num a => a
        gl_FRAMEBUFFER_ATTACHMENT_OBJECT_NAME :: Num a => a
        gl_FRAMEBUFFER_ATTACHMENT_TEXTURE_LEVEL :: Num a => a
        gl_FRAMEBUFFER_ATTACHMENT_TEXTURE_CUBE_MAP_FACE :: Num a => a
        gl_COLOR_ATTACHMENT0 :: Num a => a
        gl_DEPTH_ATTACHMENT :: Num a => a
        gl_STENCIL_ATTACHMENT :: Num a => a
        gl_DEPTH_STENCIL_ATTACHMENT :: Num a => a
        gl_NONE :: Num a => a
        gl_FRAMEBUFFER_COMPLETE :: Num a => a
        gl_FRAMEBUFFER_INCOMPLETE_ATTACHMENT :: Num a => a
        gl_FRAMEBUFFER_INCOMPLETE_MISSING_ATTACHMENT :: Num a => a
        gl_FRAMEBUFFER_INCOMPLETE_DIMENSIONS :: Num a => a
        gl_FRAMEBUFFER_UNSUPPORTED :: Num a => a
        gl_FRAMEBUFFER_BINDING :: Num a => a
        gl_RENDERBUFFER_BINDING :: Num a => a
        gl_MAX_RENDERBUFFER_SIZE :: Num a => a
        gl_INVALID_FRAMEBUFFER_OPERATION :: Num a => a
        gl_UNPACK_FLIP_Y_WEBGL :: Num a => a
        gl_UNPACK_PREMULTIPLY_ALPHA_WEBGL :: Num a => a
        gl_CONTEXT_LOST_WEBGL :: Num a => a
        gl_UNPACK_COLORSPACE_CONVERSION_WEBGL :: Num a => a
