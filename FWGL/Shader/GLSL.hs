{-# LANGUAGE ScopedTypeVariables, GADTs, RankNTypes, FlexibleContexts #-}

module FWGL.Shader.GLSL (
        vertexToGLSLAttr,
        vertexToGLSL,
        fragmentToGLSL,
        shaderToGLSL,
        exprToGLSL,
        globalName,
        attributeName
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
        let r@(_, is, _) = snd $ reduce False v
        in ( shaderToGLSL "#version 100\n" "attribute" "varying"
                          r [("hvVertexShaderOutput", "gl_Position")]
           , map (\(t, n, s) -> (n, s)) is)

vertexToGLSL :: VertexShader g i o -> String
vertexToGLSL = fst . vertexToGLSLAttr

fragmentToGLSL :: FragmentShader g i -> String
fragmentToGLSL v = shaderToGLSL "#version 100\nprecision mediump float;"
                                "varying" ""
                                (snd $ reduce True v)
                                [("hvFragmentShaderOutput", "gl_FragColor")]

shaderToGLSL :: String -> String -> String -> ShaderVars -> [(String, String)] -> String
shaderToGLSL header ins outs (gs, is, os) predec =
        header ++
        concatMap (var "uniform") gs ++
        concatMap (\(t, n, _) -> var ins (t, n)) is ++
        concatMap (\(t, n, _) -> if any ((== n) . fst) predec
                                        then []
                                        else var outs (t, n)
                  ) os ++
        "void main(){" ++
        concatMap (\(_, n, e) -> replace n predec ++ "=" ++
                                 exprToGLSL e ++ ";") os ++ "}"
        where var qual (ty, nm) = qual ++ " " ++ ty ++ " " ++ nm ++ ";"
              replace x xs = case filter ((== x) . fst) xs of
                                        ((_, y) : []) -> y
                                        _ -> x

reduce :: Bool -> Shader g i o a -> (a, ShaderVars)
reduce _ (Pure x) = (x, ([], [], []))
reduce b (Bind s f) = case reduce b s of
                        (x, (gs, is, os)) -> case reduce b $ f x of
                                (y, (gs', is', os')) ->
                                        (y, (gs ++ gs', is ++ is', os ++ os'))
reduce _ (Global :: Shader g i o a) = (fromExpr $ Read nm, ([(ty, nm)], [], []))
        where (ty, nm) = globalTypeAndName (undefined :: a)
reduce isFragment (Get :: Shader g i o a) =
        (fromExpr $ Read nm, ([], [(ty, nm, size (undefined :: a))], []))
        where (ty, nm) = (if isFragment
                                then varyingTypeAndName
                                else attributeTypeAndName) (undefined :: a)
reduce _ (Put x) = ((), ([], [], [(ty, nm, toExpr x)]))
        where (ty, nm) = varyingTypeAndName x

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

varyingTypeAndName :: (Typeable t, ShaderType t) => t -> (String, String)
varyingTypeAndName t = (typeName t, varyingName t)

attributeTypeAndName :: (Typeable t, ShaderType t) => t -> (String, String)
attributeTypeAndName t = (typeName t, attributeName t)

variableName :: Typeable t => t -> String
variableName = tyConName . typeRepTyCon . typeOf

globalName :: Typeable t => t -> String
globalName = ("hg" ++) . variableName

varyingName :: Typeable t => t -> String
varyingName = ("hv" ++) . variableName

attributeName :: Typeable t => t -> String
attributeName = ("ha" ++) . variableName
