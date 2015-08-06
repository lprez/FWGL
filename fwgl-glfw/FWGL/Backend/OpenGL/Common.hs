module FWGL.Backend.OpenGL.Common where

import Foreign
import Foreign.C.String
import Graphics.GL.Types

genToCreate :: Storable a => (GLsizei -> Ptr a -> IO ()) -> ctx -> IO a
genToCreate gen _ = do ptr <- malloc
                       gen 1 ptr
                       value <- peek ptr
                       free ptr
                       return value

deleteToDelete :: Storable a => (GLsizei -> Ptr a -> IO ()) -> ctx -> a -> IO ()
deleteToDelete del _ = flip with $ del 1

getString :: (a -> GLsizei -> Ptr GLsizei -> Ptr GLchar -> IO ())
          -> ctx -> a -> IO String
getString f _ x = do cstr <- mallocArray len
                     f x (fromIntegral len) nullPtr cstr
                     str <- peekCString cstr
                     free cstr
                     return str
        where len = 4096

uniform :: (a -> GLsizei -> Ptr b -> IO ())
        -> ctx -> a -> (GLsizei, ForeignPtr b) -> IO ()
uniform f _ a (len, fp) = withForeignPtr fp $ f a (quot len 4)

uniformMatrix :: (a -> GLsizei -> GLboolean -> Ptr b -> IO ()) -> GLsizei
              -> ctx -> a -> GLboolean -> (GLsizei, ForeignPtr b) -> IO ()
uniformMatrix f dv _ a b (len, fp) =
        withForeignPtr fp $ f a 1 {- (quot len dv) -} b

vertexAttrib :: (a -> Ptr b -> IO ())
             -> ctx -> a -> (GLsizei, ForeignPtr b) -> IO ()
vertexAttrib f _ a (_, fp) = withForeignPtr fp $ f a

mkArrayLen :: Int -> IO (GLsizei, ForeignPtr b)
mkArrayLen len = do arr <- mallocForeignPtrArray (fromIntegral len)
                           :: IO (ForeignPtr Word8)
                    return (fromIntegral len, castForeignPtr arr)

arrayToList :: Storable b => (GLsizei, ForeignPtr a) -> IO [b]
arrayToList (len, fptr) = withForeignPtr (castForeignPtr fptr) $ \ptr ->
                                peekArray (fromIntegral len) ptr

mkArray :: Storable a => [a] -> IO (GLsizei, ForeignPtr b)
mkArray xs = do arr <- mallocForeignPtrArray len
                withForeignPtr arr $ flip pokeArray xs
                return (fromIntegral size, castForeignPtr arr)
        where len = length xs
              size = len * sizeOf (head xs)
