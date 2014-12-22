{-# LANGUAGE ScopedTypeVariables, GADTs, RankNTypes, FlexibleContexts #-}

module FWGL.Shader.GLSL (
        vertexToGLSLAttr,
        vertexToGLSL,
        fragmentToGLSL,
        shaderToGLSL,
        exprToGLSL,
        globalName,
        inputName
) where

import Data.Typeable
import FWGL.Shader.Monad (Shader(..))
import FWGL.Shader.Language (Expr(..), ShaderType(..))
import FWGL.Shader.Stages (VertexShader, FragmentShader)

type ShaderVars = ( [(String, String)]
                  , [(String, String, Int)]
                  , [(String, String, Expr)])

vertexToGLSLAttr :: VertexShader g i o -> (String, [(String, Int)])
vertexToGLSLAttr v =
        let r@(_, is, _) = snd $ reduce v
        in ( shaderToGLSL r [("outVertexShaderOutput", "gl_Position")]
           , map (\(t, n, s) -> (n, s)) is)

vertexToGLSL :: VertexShader g i o -> String
vertexToGLSL = fst . vertexToGLSLAttr

fragmentToGLSL :: FragmentShader g i -> String
fragmentToGLSL v = shaderToGLSL (snd $ reduce v)
                                [("outFragmentShaderOutput", "gl_FragColor")]

shaderToGLSL :: ShaderVars -> [(String, String)] -> String
shaderToGLSL (gs, is, os) predec =
        concatMap (var "uniform") gs ++
        concatMap (\(t, n, _) -> var "in" (t, n)) is ++
        concatMap (\(t, n, _) -> if any ((== n) . fst) predec
                                        then []
                                        else var "out" (t, n)
                  ) os ++
        "void main(){" ++
        concatMap (\(_, n, e) -> replace n predec ++ "=" ++
                                 exprToGLSL e ++ ";") os ++ "}"
        where var qual (ty, nm) = qual ++ " " ++ ty ++ " " ++ nm ++ ";"
              replace x xs = case filter ((== x) . fst) xs of
                                        ((_, y) : []) -> y
                                        _ -> x

reduce :: Shader g i o a -> (a, ShaderVars)
reduce (Pure x) = (x, ([], [], []))
reduce (Bind s f) = case reduce s of
                        (x, (gs, is, os)) -> case reduce $ f x of
                                (y, (gs', is', os')) ->
                                        (y, (gs ++ gs', is ++ is', os ++ os'))
reduce (Global :: Shader g i o a) = (fromExpr $ Read nm, ([(ty, nm)], [], []))
        where (ty, nm) = globalTypeAndName (undefined :: a)
reduce (Get :: Shader g i o a) = ( fromExpr $ Read nm
                                 , ([], [(ty, nm, size (undefined :: a))], []))
        where (ty, nm) = inputTypeAndName (undefined :: a)
reduce (Put x) = ((), ([], [], [(ty, nm, toExpr x)]))
        where (ty, nm) = outputTypeAndName x

exprToGLSL :: Expr -> String
exprToGLSL Nil = ""
exprToGLSL (Read s) = s
exprToGLSL (Op1 s e) = "(" ++ s ++ exprToGLSL e ++ ")"
exprToGLSL (Op2 s x y) = "(" ++ exprToGLSL x ++ s ++ exprToGLSL y ++ ")"
exprToGLSL (Apply s es) = s ++ "(" ++ tail (es >>= (',' :) . exprToGLSL) ++ ")"
exprToGLSL (X x) = exprToGLSL x ++ ".x"
exprToGLSL (Y x) = exprToGLSL x ++ ".y"
exprToGLSL (Z x) = exprToGLSL x ++ ".z"
exprToGLSL (W x) = exprToGLSL x ++ ".w"
exprToGLSL (Literal s) = s

globalTypeAndName :: (Typeable t, ShaderType t) => t -> (String, String)
globalTypeAndName t = (typeName t, globalName t)

inputTypeAndName :: (Typeable t, ShaderType t) => t -> (String, String)
inputTypeAndName t = (typeName t, inputName t)

outputTypeAndName :: (Typeable t, ShaderType t) => t -> (String, String)
outputTypeAndName t = (typeName t, outputName t)

variableName :: Typeable t => t -> String
variableName = tyConName . typeRepTyCon . typeOf

globalName :: Typeable t => t -> String
globalName = ("global" ++) . variableName

inputName :: Typeable t => t -> String
inputName = ("in" ++) . inputName

outputName :: Typeable t => t -> String
outputName = ("out" ++) . inputName
