{-# LANGUAGE MultiParamTypeClasses, ExistentialQuantification,
             FunctionalDependencies, KindSignatures, DataKinds,
             RankNTypes, FlexibleInstances, ScopedTypeVariables,
             TypeOperators, ImpredicativeTypes, TypeSynonymInstances #-}

module FWGL.Shader.Program (
        LoadedProgram(..),
        Program,
        program,
        loadProgram,
        castProgram,

        -- TODO: move
        module FWGL.Shader.Default,
        Subset,
        Union,
        Insert,
        DefaultUniforms,
        DefaultAttributes,
        defaultProgram,
) where

import Data.Hashable
import qualified Data.HashMap.Strict as H
import Data.Word (Word)
import FWGL.Shader.Default (Transform3, View3, Texture2,
                            Position3, Normal3, UV,
                            defaultVertexShader, defaultFragmentShader)
import FWGL.Shader.GLSL
import FWGL.Shader.Monad (Subset, Union, Insert)
import FWGL.Shader.Stages
import FWGL.Internal.GL hiding (Program)
import qualified FWGL.Internal.GL as GL
import FWGL.Internal.Resource
import Unsafe.Coerce

data Program (gs :: [*]) (is :: [*]) =
        Program (String, [(String, Int)]) String Int

data LoadedProgram = LoadedProgram !GL.Program (H.HashMap String Int) Int

type DefaultUniforms = '[Transform3, View3, Texture2]
type DefaultAttributes = '[Position3, UV, Normal3]

instance Hashable (Program gs is) where
        hashWithSalt salt (Program _ _ h) = hashWithSalt salt h

instance Eq (Program gs is) where
        (Program _ _ h) == (Program _ _ h') = h == h'

instance Hashable LoadedProgram where
        hashWithSalt salt (LoadedProgram _ _ h) = hashWithSalt salt h

instance Eq LoadedProgram where
        (LoadedProgram _ _ h) == (LoadedProgram _ _ h') = h == h'

instance GLES => Resource (Program g i) LoadedProgram GL where
        loadResource = loadProgram
        unloadResource _ (LoadedProgram p _ _) = deleteProgram p

castProgram :: Program gs is -> Program gs' is'
-- castProgram (Program v f h) = Program v f h
castProgram = unsafeCoerce

program :: (Subset gs' gs, Subset gs'' gs, Subset os' os)
        => VertexShader gs' is os -> FragmentShader gs'' os'
        -> Program gs is
program vs fs = let (vss, attrs) = vertexToGLSLAttr vs
                    fss = fragmentToGLSL fs
                in Program (vss, attrs) fss (hash (vss, fss))

defaultProgram :: Program DefaultUniforms DefaultAttributes
defaultProgram = program defaultVertexShader defaultFragmentShader

loadProgram :: GLES => Program g i -> GL LoadedProgram
loadProgram (Program (vss, attrs) fss h) = do
        glp <- createProgram

        vs <- loadSource gl_VERTEX_SHADER vss
        fs <- loadSource gl_FRAGMENT_SHADER fss
        attachShader glp vs
        attachShader glp fs

        locs <- bindAttribs glp 0 attrs []
        linkProgram glp

        -- TODO: ??
        {-
        detachShader glp vs
        detachShader glp fs
        -}

        return $ LoadedProgram glp (H.fromList locs) h

        where bindAttribs _ _ [] r = return r
              bindAttribs glp i ((nm, sz) : xs) r =
                        bindAttribLocation glp (fromIntegral i) (toGLString nm)
                        >> bindAttribs glp (i + sz) xs ((nm, i) : r)

loadSource :: GLES => GLEnum -> String -> GL Shader
loadSource ty src =
        do shader <- createShader ty
           shaderSource shader $ toGLString src
           compileShader shader
           return shader
