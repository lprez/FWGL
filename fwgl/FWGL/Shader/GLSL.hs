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
        actionGenerator :: ActionGenerator
        actionDeps :: ActionSet,
        actionContext :: ActionContext
}

type ActionGenerator = String -> String

-- | The context is where an action should be put. Only for-loops are considered
-- contexts, because there is no reason to put some action inside another block.
-- Of course, an action could have many contexts (e.g. for(..) { for (..) {
-- act; } }), but only one is actually needed to compile the action.
data ActionContext = ShallowContext ActionSet       -- ^ The contexts of the expressions used in the action.
                   | DeepContext ActionSet          -- ^ All the contexts (including those of the dependencies).
                   | NoContext                      -- ^ Root action.
                   | LeastContext Int ActionID      -- ^ Depth and smallest context.

type ActionGraph = H.HashMap ActionID ActionInfo

-- | Compile a list of 'Expr', sharing their actions.
compile :: [Expr] -> [String]

generate :: ActionGenerator -> Maybe ActionGraph -> String
generate gen Nothing = gen ""
generate gen (Just graph) = gen $ sortActions graph >>= uncurry generate

sortActions :: ActionGraph -> [(ActionGenerator, Maybe ActionGraph)]
sortActions g = sortActions' (H.empty, [], listToMaybe $ H.keys g, g)
        where sortActions' (childrenMap, sortedAids, aid, graph) = 
                let ai = graph H.! aid
                    nextAid = head $ H.keys graph
                in case actionContext ai of
                        LeastContext _ parentId ->
                                let cmap' = H.insertWith H.union
                                                         parentId 
                                                         (H.singleton aid ai)
                                in sortActions (cmap', sortedAids, nextAid, graph)
                        NoContext ->
                                let (cmap', sa', _, graph') =
                                        sortActions' (childrenMap, sortedAids, 
                                in (aid ++ 

-- | Build an action graph with full contexts. It is both a graph (with
-- dependencies as edges) and a tree (with contexts).
buildHierarchicalGraph :: ActionMap -> ActionGraph
buildHierarchicalGraph =
        contextAll depth . contextAll deep . buildActionGraph

-- | Build an action graph with shallow contexts.
buildActionGraph :: ActionMap -> ActionGraph
buildActionGraph = flip foldrWithKey H.empty $
        \aid act graph ->
                let (info, deps) = compileAction act
                in H.union (H.insert aid info graph)
                           (buildActionGraph deps)

-- | Transform every context.
contextAll :: (ActionID -> ActionGraph -> (ActionContext, ActionGraph))
           -> ActionGraph -> ActionGraph
contextAll f g = foldrWithKey (\aid _ graph -> snd $ f aid graph) g g

-- | Calculate the depth of the contexts and pick the smallest context.
depth :: ActionID -> ActionGraph -> (ActionContext, ActionGraph)
depth aid graph =
        case actionContext act of
                ShallowContext _ -> error "depth: action must be contextualized"
                DeepContext ctx -> updateContext $
                        H.foldrWithKey accumDepth (NoContext, graph) ctx
                ctx -> (ctx, graph)
        where act = graph H.! aid
              updateContext (ctx, graph) =
                      H.insert aid (act { actionContext = ctx }) graph

              depthNum NoContext = 0
              depthNum (LeastContext n _) = n

              accumDepth aid' act' (ctx, graph) =
                      let dep = depthNum ctx
                          (ctx', graph') = depth aid' graph
                          dep' = depthNum ctx'
                      in if dep <= dep'
                             then (LeastContext (dep' + 1) aid', graph')
                             else (ctx, graph')

-- | Find and build the deep context of this action. Returns a deep context and
-- a new graph with the deep contextes of this action and of its dependencies.
deep :: ActionID -> ActionGraph -> (ActionContext, ActionGraph)
deep aid graph =
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
                      let (DeepContext dCtx, graph') = deep aid graph 
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
compileAction aid (Store ty expr) =
        let (eStr, deps, ctxs) = compileExpr expr
        in ( Action (\c -> concat [ c, ty, " ", actionName aid
                                  , "=", eStr, ";" ])
                    (H.map (const ()) deps) (ShallowContext ctxs)
           , deps )

compileAction aid (If cExpr ty tExpr fExpr) =
        let (cStr, cDeps, cCtxs) = compileExpr cExpr
            (tStr, tDeps, tCtxs) = compileExpr tExpr
            (fStr, fDeps, fCtxs) = compileExpr fExpr
            deps = H.unions [cDeps, tDeps, fDeps]
            name = actionName aid
        in ( Action (\c -> concat [ ty, " ", name, "=", toExpr zero, ";if("
                                  , cStr, "){", c, name, "=", tStr, ";}else{"
                                  , name, "fStr", ";}" ])
                    (H.map (const ()) deps)
                    (ShallowContext $ H.unions [cCtxs, tCtxs, fCtxs])
           , deps )

compileAction aid (For iters ty initVal body) =
        let iterName = contextVarName LoopIteration aid
            valueName = contextVarName LoopValue aid
            (nExpr, sExpr) = body (ContextVar aid LoopIteration)
                                  (ContextVar aid LoopValue)
            (iStr, iDeps, iCtxs) = compileExpr iExpr
            (nStr, nDeps, nCtxs) = compileExpr nExpr
            (sStr, sDeps, sCtxs) = compileExpr sExpr
            deps = H.unions [iDeps, nDeps, sDeps]
        in ( Action (\c -> concat [ ty, " ", valueName, "=", iStr, ";"
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
