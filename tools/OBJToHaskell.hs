module Main where

import FWGL.Geometry.OBJ
import System.Environment

var :: FilePath -> String -> IO String
var fname varName = do geom <- fmap (show . geometryOBJ) $ loadOBJ fname
                       return $ varName ++ " :: Geometry\n" ++
                                varName ++ " = " ++ geom

main :: IO ()
main = getArgs >>= \args ->
        case args of
                (fn : []) -> var fn (takeWhile (/= '.') fn) >>= putStrLn
                (fn : vn : []) -> var fn vn >>= putStrLn
                (fn : vn : mn : _) -> do putStrLn $ "module " ++ mn ++ 
                                                    " (" ++ vn ++ ") where \n\
                                                    \\nimport FWGL.Geometry\n\
                                                    \import FWGL.Vector\n"
                                         var fn vn >>= putStrLn
                [] -> putStrLn "Usage: OBJToHaskell model.obj \
                               \[variableName [moduleName]] [>file.hs]"
