{-# LANGUAGE GADTs, DataKinds, KindSignatures, TypeOperators,
             RankNTypes, FlexibleContexts #-}

module FWGL.Shader.Monad (
        Shader(..),
        PartialShader,
        Member,
        AllTypeable,
        Subset,
        Equal,
        Union,
        Insert,
        return,
        get,
        global,
        put,
        (>>),
        (>>=),
        fail
) where

import Data.Typeable
import FWGL.Internal.TList
import FWGL.Shader.Language (ShaderType)
import Prelude (String, error)

data Shader g i o a where
        Pure :: a -> Shader g i o a
        Bind :: Shader g' i' o' b
             -> (b -> Shader g'' i'' o'' a)
             -> Shader g i o a
        Get :: (Member a i, Typeable a, ShaderType a) => Shader g i o a
        Global :: (Member a g, Typeable a, ShaderType a) => Shader g i o a
        -- Put :: (Typeable a, ShaderType a) => a -> Shader g i (a ': o) ()
        Put :: (Member a o, Typeable a, ShaderType a) => a -> Shader g i o ()

type PartialShader g i o a =
        (Subset o o', Subset g g', Subset i i', Subset i' i)
        => Shader g' i' o' a

-- TODO: Shader è una monade e un funtore, non c'è bisogno di rebindare la
-- sintassi
return :: a -> Shader g i o a
return = Pure

get :: (Member a i, Typeable a, ShaderType a) => Shader g i o a
get = Get

global :: (Member a g, Typeable a, ShaderType a) => Shader g i o a
global = Global

put :: (Member a o, Typeable a, ShaderType a) => a -> Shader g i o ()
put = Put

fail :: String -> Shader g i o a
fail = error

(>>=) :: Shader g i o a -> (a -> Shader g i o b) -> Shader g i o b
(>>=) = Bind

(>>) :: Shader g i o a -> Shader g i o b -> Shader g i o b
x >> y = x >>= \_ -> y

class AllTypeable (xs :: [*])
instance AllTypeable '[]
instance (Typeable x, AllTypeable xs) => AllTypeable (x ': xs)
