{-# LANGUAGE GADTs, ScopedTypeVariables #-}
module Main where

import FWGL.Geometry
import FWGL.Geometry.OBJ
import FWGL.Vector
import System.Environment

var :: FilePath -> String -> IO String
var fname varName = do (v, u, n, es) <- fmap attributesOBJ $ loadOBJ fname
                       return $ varName ++ " :: GLES => Geometry Geometry3\n" ++
                                varName ++ " = mkGeometry3 " ++ show v ++ " " ++
                                show u ++ " " ++ show n ++ " " ++ show es
                       {-
                       -- TODO: import FWGL.Backend.GLFW.* and use this
                       let Geometry _ _ h = mkGeometry3 v u n es
                       return $ varName ++ " :: Geometry Geometry3\n" ++
                                varName ++ " = Geometry (" ++ 
                                "AttrListCons undefined " ++ show v ++
                                "$ AttrListCons undefined " ++ show u ++
                                "$ AttrListCons undefined " ++ show n ++
                                "AttrListNil) " ++ show es ++ " " ++ show h
                       -}

main :: IO ()
main = getArgs >>= \args ->
        case args of
                (fn : []) -> var fn (takeWhile (/= '.') fn) >>= putStrLn
                (fn : vn : []) -> var fn vn >>= putStrLn
                (fn : vn : mn : _) -> do putStrLn $ "module " ++ mn ++ 
                                                    " (" ++ vn ++ ") where \n\
                                                    \\nimport FWGL.Backend\n\
                                                    \import FWGL.Geometry\n\
                                                    \import FWGL.Vector\n"
                                         var fn vn >>= putStrLn
                [] -> putStrLn "Usage: OBJToHaskell model.obj \
                               \[variableName [moduleName]] [>file.hs]"
