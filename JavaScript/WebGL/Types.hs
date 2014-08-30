{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, TypeSynonymInstances #-}

module JavaScript.WebGL.Types (
        Ctx,
        Float32Array,
        Int32Array,
        Uint16Array,
        Program,
        Shader,
        Buffer,
        FrameBuffer,
        RenderBuffer,
        Texture,
        UniformLocation,
        ActiveInfo,
        ShaderPrecisionFormat,
        ArrayBufferView,
        getCtx,
        float32Array,
        int32Array,
        uint16Array,
        float32View,
        int32View,
        uint16View,
        noBuffer
) where

import Data.Int (Int32)
import Data.Word (Word16)
import GHCJS.Foreign
import GHCJS.Types

data Ctx_
type Ctx = JSRef Ctx_

data Float32Array_
type Float32Array = JSRef Float32Array_

data Int32Array_
type Int32Array = JSRef Int32Array_

data Uint16Array_
type Uint16Array = JSRef Uint16Array_

data Program_
type Program = JSRef Program_

data Shader_
type Shader = JSRef Shader_

data Buffer_
type Buffer = JSRef Buffer_

data FrameBuffer_
type FrameBuffer = JSRef FrameBuffer_

data RenderBuffer_
type RenderBuffer = JSRef RenderBuffer_

data Texture_
type Texture = JSRef Texture_

data UniformLocation_
type UniformLocation = JSRef UniformLocation_

data ActiveInfo_
type ActiveInfo = JSRef ActiveInfo_

data ShaderPrecisionFormat_
type ShaderPrecisionFormat = JSRef ShaderPrecisionFormat_

data ArrayBufferView_
type ArrayBufferView = JSRef ArrayBufferView_

--instance (ArrayBufferView a b) => Image a
--instance Image ImageData
--instance Image ImageElement

noBuffer :: Buffer
noBuffer = jsNull

float32View :: JSArray Float -> IO ArrayBufferView
float32View = fmap castRef . float32Array

int32View :: JSArray Int32 -> IO ArrayBufferView
int32View = fmap castRef . int32Array

uint16View :: JSArray Word16 -> IO ArrayBufferView
uint16View = fmap castRef . uint16Array

foreign import javascript unsafe "$r = new Float32Array($1);"
        float32Array :: JSArray Float -> IO Float32Array

foreign import javascript unsafe "$r = new Int32Array($1);"
        int32Array :: JSArray Int32 -> IO Int32Array

foreign import javascript unsafe "$r = new Uint16Array($1);"
        uint16Array :: JSArray Word16 -> IO Uint16Array

foreign import javascript unsafe "$r = $1.getContext(\"webgl\");"
        getCtx :: JSRef a -> IO Ctx
