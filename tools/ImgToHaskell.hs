module Main where

import Codec.Picture
import Codec.Picture.Types
import FWGL.Graphics.Color
import FWGL.Texture
import System.Environment

imgRGBA8 :: DynamicImage -> Image PixelRGBA8
imgRGBA8 (ImageY8 img) = promoteImage img
imgRGBA8 (ImageYA8 img) = promoteImage img
imgRGBA8 (ImageRGB8 img) = promoteImage img
imgRGBA8 (ImageRGBA8 img) = img
imgRGBA8 _ = error "unsupported pixel format"

loadTexture :: FilePath -> IO Texture
loadTexture fname = do eimg <- readImage fname
                       case eimg of
                               Left s -> error s
                               Right dimg ->
                                       let w = imageWidth i
                                           h = imageHeight i
                                           i = imgRGBA8 dimg
                                       in return . mkTexture w h $
                                               map (color . uncurry (pixelAt i))
                                                   [ (x, y) | y <- [0 .. h - 1]
                                                            , x <- [0 .. w - 1]
                                                   ]
        where color (PixelRGBA8 r g b a) = Color r g b a


var :: FilePath -> String -> IO String
var fname varName = do tex <- fmap show $ loadTexture fname
                       return $ varName ++ " :: Texture\n" ++
                                varName ++ " = " ++ tex

main :: IO ()
main = getArgs >>= \args ->
        case args of
                (fn : []) -> var fn (takeWhile (/= '.') fn) >>= putStrLn
                (fn : vn : []) -> var fn vn >>= putStrLn
                (fn : vn : mn : _) -> do putStrLn $ "module " ++ mn ++ 
                                                    " (" ++ vn ++ ") where \n\
                                                    \\nimport FWGL.Graphics\
                                                                 \.Color\n\
                                                    \import FWGL.Texture\n"
                                         var fn vn >>= putStrLn
                [] -> putStrLn "Usage: ImgToHaskell image \
                               \[variableName [moduleName]] [>file.hs]"
