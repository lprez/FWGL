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
import Control.Monad.Trans.Class
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

type ActionID = Int
type ActionMap = H.HashMap ActionID Action
type ActionSet = H.HashMap ActionID ()

data ActionInfo = ActionInfo {
        action :: Action,
        actionString :: String -> String,
        actionDeps :: ActionSet,
        actionContext :: ActionContext,
        actionFullContext :: Bool
}

-- | The context is where an action should be put. Only for-loops are considered
-- contexts, because there is no reason to put some action inside another block.
-- Of course, an action could have many contexts (e.g. for(..) { for (..) {
-- act; } }), but only one is actually needed to compile the action. Shallow and
-- deep contexts are the precursors to the real context (LowestContext).
data ActionContext = ShallowContext ActionSet       -- ^ The contexts of the expressions used in the action.
                   | DeepContext ActionSet          -- ^ All the contexts (including those of the dependencies).
                   | LowestContext (Maybe ActionID) -- ^ The lowest context.

type ActionGraph = H.HashMap ActionID ActionInfo
-- type ActionCompiler = State ActionGraph

-- | Build an action graph with full contexts.
preCompileActions :: ActionMap -> ActionGraph
preCompileActions = findLCAs . contextualizeAll . buildActionGraph

-- | Build an action graph with shallow contexts.
buildActionGraph :: ActionMap -> ActionGraph
buildActionGraph = flip foldrWithKey H.empty $
        \aid act graph ->
                let (info, deps) = compileAction act
                in H.union (H.insert aid info graph)
                           (buildActionGraph deps)

-- |Turn 'ShallowContext's into 'DeepContext's. 
contextualizeAll :: ActionGraph -> ActionGraph
contextualizeAll g = foldrWithKey
                        (\aid _ graph -> snd $ contextualize aid graph) g g

-- | Turn 'DeepContext's into 'LowestContext's.
findLCAs :: ActionGraph -> ActionGraph


-- | Find and build the deep context of this action. Returns a deep context and
-- a new graph with the deep contextes of this action and of its dependencies.
contextualize :: ActionID -> ActionGraph -> (ActionContext, ActionGraph)
contextualize aid graph =
        case actionContext act of
                ShallowContext sctx ->
                        let (dctx, graph') = H.foldrWithKey addDepContext
                                                            (sctx, graph)
                                                            (actionDeps sctx)
                        in ( DeepContext dctx
                           , H.insert aid
                                      (act { actionContext = DeepContext dctx })
                                      graph' )
                ctx -> (ctx, graph)
        where act = graph H.! aid
              addDepContext depId depInfo (ctx, graph) = 
                      -- XXX
                      let (DeepContext dCtx, graph') = contextualize aid graph 
                      in (H.union ctx dCtx, graph')

-- | Compile an 'Expr'. Returns the compiled expression, the map of dependencies
-- and the context.
compileExpr :: Expr -> (String, ActionMap, ActionSet)
compileExpr Empty = ("", H.empty, H.empty)
compileExpr (Read s) = (s, H.empty, H.empty)

compileExpr (Op1 s e) = let (x, a, c) = compileExpr e
                        in ("(" ++ s ++ x ++ ")", a, c)

compileExpr (Op2 s ex ey) = let (x, ax, cx) = compileExpr ex
                                (y, ay, cy) = compileExpr ey
                            in ( "(" ++ x ++ s ++ y ++ ")"
                               , H.union ax ay, H.union cx cy )

compileExpr (Apply s es) = let (vs, as, cs) = unzip3 $ map compileExpr es
                           in ( concat $ [ s, "(" , tail (vs >>= (',' :)), ")" ]
                              , H.unions as, H.unions cs)

compileExpr (X e) = (compileExpr e ++ "[0]", H.empty, H.empty)
compileExpr (Y e) = (compileExpr e ++ "[1]", H.empty, H.empty)
compileExpr (Z e) = (compileExpr e ++ "[2]", H.empty, H.empty)
compileExpr (W e) = (compileExpr e ++ "[3]", H.empty, H.empty)
compileExpr (Literal s) = (s, H.empty, H.empty)
compileExpr (Action a) = let h = hash a
                         in (actionName h, H.singleton h a, H.empty)
compileExpr (ContextVar i t) = (contextVarName t i, H.empty, H.singleton i ())
compileExpr (Dummy _) = error "compileExpr: Dummy"

compileAction :: ActionID -> Action -> (ActionInfo, ActionMap)
compileAction aid a@(Store ty expr) =
        let (eStr, deps, ctxs) = compileExpr expr
        in ( Action a (\c -> concat [ c, ty, " ", actionName aid
                                    , "=", eStr, ";" ])
                    (H.map (const ()) deps) (ShallowContext ctxs)
           , deps )

compileAction aid a@(If cExpr ty tExpr fExpr) =
        let (cStr, cDeps, cCtxs) = compileExpr cExpr
            (tStr, tDeps, tCtxs) = compileExpr tExpr
            (fStr, fDeps, fCtxs) = compileExpr fExpr
            deps = H.unions [cDeps, tDeps, fDeps]
            name = actionName aid
        in ( Action a
                    (\c -> concat [ ty, " ", name, "=", toExpr zero, ";if("
                                  , cStr, "){", c, name, "=", tStr, ";}else{"
                                  , name, "fStr", ";}" ])
                    (H.map (const ()) deps)
                    (ShallowContext $ H.unions [cCtxs, tCtxs, fCtxs])
           , deps )

compileAction aid a@(For iters ty initVal body) =
        let iterName = contextVarName LoopIteration aid
            valueName = contextVarName LoopValue aid
            (nExpr, sExpr) = body (ContextVar aid LoopIteration)
                                  (ContextVar aid LoopValue)
            (iStr, iDeps, iCtxs) = compileExpr iExpr
            (nStr, nDeps, nCtxs) = compileExpr nExpr
            (sStr, sDeps, sCtxs) = compileExpr sExpr
            deps = H.unions [iDeps, nDeps, sDeps]
        in ( Action a
                    (\c -> concat [ ty, " ", valueName, "=", iStr, ";"
                                  , "for(float ", iterName, "=0.0;"
                                  , iterName, "<", show iters, ".0;"
                                  , "++", iterName, "){", c
                                  , "if(", sStr, "){break;}"
                                  , valueName, "=", nStr, ";}" ])
                    (H.map (const ()) deps)
                    (ShallowContext $ H.unions [iCtxs, nCtxs, sCtxs])
           , deps )

actionName :: ActionID -> String
actionName = ('a' :) . show

contextVarName :: ContextVarType -> ActionID -> String
contextVarName LoopIteration = ('l' :) . show
contextVarName LoopValue = actionName

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
