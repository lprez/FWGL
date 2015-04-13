{-# LANGUAGE ScopedTypeVariables, GADTs, RankNTypes, FlexibleContexts,
             KindSignatures, DataKinds, TypeOperators, ConstraintKinds #-}

module FWGL.Shader.GLSL (
        vertexToGLSLAttr,
        vertexToGLSL,
        fragmentToGLSL,
        shaderToGLSL,
        globalName,
        attributeName
) where

import Control.Monad
import Control.Monad.Trans.State
import qualified Data.HashMap.Strict as H
import Data.Typeable
import FWGL.Shader.Shader
import FWGL.Shader.Language (Expr(..), ShaderType(..), AM(..), Unknown)
import FWGL.Shader.Stages (VertexShader, FragmentShader, ValidVertex)

type ShaderVars = ( [(String, String)]
                  , [(String, String, Int)]
                  , [(String, String, Expr)])

vertexToGLSLAttr :: ValidVertex g i o => VertexShader g i o
                 -> (String, [(String, Int)])
vertexToGLSLAttr v =
        let r@(_, is, _) = vars False v
        in ( shaderToGLSL "#version 100\n" "attribute" "varying"
                          r [("hvVertexShaderOutput", "gl_Position")]
           , map (\(t, n, s) -> (n, s)) is)

vertexToGLSL :: ValidVertex g i o => VertexShader g i o -> String
vertexToGLSL = fst . vertexToGLSLAttr

fragmentToGLSL :: Valid g i '[] => FragmentShader g i -> String
fragmentToGLSL v = shaderToGLSL "#version 100\nprecision mediump float;"
                                "varying" ""
                                (vars True v)
                                [("hvFragmentShaderOutput", "gl_FragColor")]

shaderToGLSL :: String -> String -> String -> ShaderVars -> [(String, String)] -> String
shaderToGLSL header ins outs (gs, is, os) predec = concat
        [ header
        , concatMap (var "uniform") gs
        , concatMap (\(t, n, _) -> var ins (t, n)) is
        , concatMap (\(t, n, _) -> if any ((== n) . fst) predec
                                          then []
                                          else var outs (t, n)
                    ) os
        , "void main(){"
        , runAction $ mapM_ (\(_, n, e) -> Set (replace n predec)
                                               (fromExpr e :: Unknown)
                            ) os
        , "}" ]
        where var qual (ty, nm) = qual ++ " " ++ ty ++ " " ++ nm ++ ";"
              replace x xs = case filter ((== x) . fst) xs of
                                        ((_, y) : []) -> y
                                        _ -> x

vars :: Valid gs is os => Bool -> Shader gs is os -> ShaderVars
vars isFragment (shader :: Shader gs is os) =
        ( staticList (undefined :: Proxy gs) globalTypeAndName
        , staticList (undefined :: Proxy is) inputVar
        , stFold (\acc x -> outputVar x : acc) [] outputs)
        where outputs = shader (staticSTList (undefined :: Proxy gs) globalExpr)
                               (staticSTList (undefined :: Proxy is) inputExpr)

              globalExpr, inputExpr :: (Typeable x, ShaderType y) => x -> y
              globalExpr x = fromExpr . Read $ globalName x
              inputExpr x = fromExpr . Read $ if isFragment then varyingName x
                                               else attributeName x

              inputVar :: (Typeable x, ShaderType x)
                       => x -> (String, String, Int)
              inputVar x = let (ty, nm) = if isFragment
                                                then varyingTypeAndName x
                                                else attributeTypeAndName x
                           in (ty, nm, size x)

              outputVar :: (Typeable x, ShaderType x)
                        => x -> (String, String, Expr)
              outputVar x = let (ty, nm) = varyingTypeAndName x
                            in (ty, nm, toExpr x)

type ActionMap = H.HashMap Int (AM Expr)
type ActionStringMap = H.HashMap Int String

compileExpr :: Expr -> State ActionMap String
compileExpr Empty = return ""
compileExpr (Read s) = return s

compileExpr (Op1 s e) = compileExpr e >>= \x -> return $ "(" ++ s ++ x ++ ")"

compileExpr (Op2 s ex ey) = do x <- compileExpr ex
                               y <- compileExpr ey
                               return $ "(" ++ x ++ s ++ y ++ ")"

compileExpr (Apply s es) = do vs <- mapM compileExpr es
                              return . concat $ 
                                [ s, "(" , tail (vs >>= (',' :)), ")" ]

compileExpr (X e) = compileExpr e >>= \x -> return $ x ++ ".x"
compileExpr (Y e) = compileExpr e >>= \x -> return $ x ++ ".y"
compileExpr (Z e) = compileExpr e >>= \x -> return $ x ++ ".z"
compileExpr (W e) = compileExpr e >>= \x -> return $ x ++ ".w"
compileExpr (Literal s) = return s
compileExpr (Action i a) = do modify $ H.insert i a
                              return $ "a" ++ show i

type ActionCompiler = State (ActionStringMap, [Int], Int)

runAction :: AM a -> String
runAction = runActionCompiler . fmap snd . compileAction

runActionCompiler :: ActionCompiler String -> String
runActionCompiler c =
        let (mainAct, (preActs, order, _)) = runState c (H.empty, [], 0)
        in concatMap (\idx -> preActs H.! idx) (reverse order) ++ mainAct

compileAction :: AM a -> ActionCompiler (a, String)
compileAction (Pure x) = return (x, "")

compileAction (Bind ax f) = do (xr, xs) <- compileAction ax
                               (yr, ys) <- compileAction $ f xr
                               return (yr, xs ++ ys)

compileAction (Var x) = do initValue <- expr $ toExpr x
                           name <- varName
                           return ( (fromExpr $ Read name, Set name)
                                  , concat [ typeName x
                                           , " ", name
                                           , "=", initValue, ";" ]
                                  )

compileAction (If condValue actionTrue actionFalse) =
        do (_, true) <- compileAction actionTrue
           (_, false) <- compileAction actionFalse
           cond <- expr $ toExpr condValue
           return ((), concat [ "if(", cond, "){", true, "}else{", false, "}" ])

compileAction (For actionInit condValue actionNext actionBody) =
        do (_, init) <- compileAction actionInit
           (_, next) <- compileAction actionNext
           (_, body) <- compileAction actionBody
           cond <- expr $ toExpr condValue
           return ((), concat [ "for(", init, ";", cond, ";", next, "){" 
                              , body, "}" ])

varName :: ActionCompiler String
varName = get >>= \(a, d, i) -> put (a, d, i + 1) >> return ("a" ++ show i)

expr :: Expr -> ActionCompiler String
expr e = let (compiledExpr, exprActionMap) = runState (compileExpr e) H.empty
         in do H.traverseWithKey
                (\depIndex depAction ->
                        do (curStringMap, curIndices, _) <- get
                           when (not $ H.member depIndex curStringMap) $
                                do (_, actionString) <- compileAction depAction
                                   modify $ \(newStringMap, newIndices, i) ->
                                        ( H.insert depIndex actionString
                                                   newStringMap
                                        , depIndex : newIndices, i )
                ) exprActionMap
               return compiledExpr

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
