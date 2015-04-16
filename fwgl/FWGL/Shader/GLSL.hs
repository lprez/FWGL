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
import FWGL.Shader.Language (Expr(..), ShaderType(..), AM(..),
                             Unknown)
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

-- | Uncompiled actions.
type ActionMap = H.HashMap Int (AM Expr)

-- | Compiled actions.
type ActionStringMap = H.HashMap Int String

-- | Compile an 'Expr', preserving its action dependencies in the state.
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

compileExpr (X e) = compileExpr e >>= \x -> return $ x ++ "[0]"
compileExpr (Y e) = compileExpr e >>= \x -> return $ x ++ "[1]"
compileExpr (Z e) = compileExpr e >>= \x -> return $ x ++ "[2]"
compileExpr (W e) = compileExpr e >>= \x -> return $ x ++ "[3]"
compileExpr (Literal s) = return s
compileExpr (Action i a) = do modify $ H.insert i a
                              return $ "a" ++ show i

-- | The state of the 'ActionCompiler'.
data ActionCompilerState =
        ActionCompilerState {
                -- | Dependencies of the parent action (they won't be compiled
                -- in this action).
                parentDeps :: ActionStringMap,

                -- | Dependencies of the current action.
                currentDeps :: ActionStringMap,

                -- | Sequence of the dependencies.
                depOrder :: [Int],

                -- | For 'genName'.
                varIndex :: Int
        }

type ActionCompiler = State ActionCompilerState

-- | Compile an action with its dependencies, ignoring the returned value.
runAction :: AM a -> String
runAction = snd . runActionCompiler . compileAction

-- | Fully evaluate a partially compiled action, appending its dependencies
-- to the top of the result string.
runActionCompiler :: ActionCompiler (a, String) -> (a, String)
runActionCompiler c =
        let ((r, mainAct), state) =
                runState c ActionCompilerState {
                        parentDeps = H.empty,
                        currentDeps = H.empty,
                        depOrder = [],
                        varIndex = 0 }
        in (r, concatMap (\idx -> currentDeps state H.! idx)
                         (reverse $ depOrder state) ++ mainAct)

-- | Partially compile a single action, preserving its dependencies in the
-- state.
compileAction :: AM a -> ActionCompiler (a, String)
compileAction (Pure x) = return (x, "")

compileAction (Bind ax f) = do (xr, xs) <- compileAction ax
                               (yr, ys) <- compileAction $ f xr
                               return (yr, xs ++ ys)

compileAction (Var mName x) = do initValue <- actionExpr $ toExpr x
                                 name <- case mName of
                                                Just n -> return n
                                                Nothing -> genName
                                 return ( (fromExpr $ Read name, Set name)
                                        , concat [ typeName x
                                                 , " ", name
                                                 , "=", initValue, ";" ]
                                        )

compileAction (Set name value) = do str <- actionExpr $ toExpr value
                                    return ((), concat [ name, "=", str, ";" ])

compileAction (If condValue actionTrue actionFalse) =
        do (_, true) <- compileSubAction actionTrue
           (_, false) <- compileSubAction actionFalse
           cond <- actionExpr $ toExpr condValue
           return ((), concat [ "if(", cond, "){", true, "}else{", false, "}" ])

compileAction (For repeats action) =
        do i <- genName
           (_, body) <- compileSubAction . action . fromExpr $ Read i
           return ((), concat [ "for(float ", i, "=0.0;", i, "<", show repeats
                              , ".0", ";++", i, "){", body, "}" ])

compileAction Break = return ((), "break;")

-- | Fully compile an action with the dependencies of the current
-- 'ActionCompiler' as parent dependencies.
compileSubAction :: AM a -> ActionCompiler (a, String)
compileSubAction act =
        do parentState <- get

           let ((i, r), actionString) = runActionCompiler $
                 do modify $ \s -> s {
                         parentDeps = currentDeps parentState,
                         varIndex = varIndex parentState }
                    (r, str) <- compileAction act
                    i <- fmap varIndex get
                    return ((i, r), str)

           put parentState { varIndex = i }
           return (r, actionString)

-- | Fully compile a sub-action with its separate dependencies and add it as a
-- dependency to the current action.
addDependency :: Int -> AM a -> ActionCompiler ()
addDependency idx act =
        do (_, actionString) <- compileSubAction act
           state <- get
           let deps = currentDeps state

           when (not $ H.member idx (parentDeps state) ||
                       H.member idx deps) $
                put state
                        { currentDeps = H.insert idx actionString deps
                        , depOrder = idx : depOrder state }

-- | Generate a new variable name in the 'ActionCompiler'.
genName :: ActionCompiler String
genName = get >>= \s -> let i = varIndex s
                        in put s { varIndex = i + 1 } >> return ("v" ++ show i)

-- | Compile an 'Expr', adding its possible action dependencies.
actionExpr :: Expr -> ActionCompiler String
actionExpr e = let (compiledExpr, exprActionMap) = runState (compileExpr e)
                                                   H.empty
               in do H.traverseWithKey
                       (\depIndex depAction -> addDependency depIndex depAction)
                       exprActionMap
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
