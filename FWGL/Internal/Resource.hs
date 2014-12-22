{-# LANGUAGE ExistentialQuantification, MultiParamTypeClasses,
             FunctionalDependencies #-}

module FWGL.Internal.Resource (
        ResMap,
        Resource(..),
        newResMap,
        addResource,
        getResource,
        removeResource
) where

import Control.Applicative
import Data.Functor
import qualified Data.HashMap.Strict as H
import Data.Hashable

data ResMap i r = forall m. Resource i r m =>
                            ResMap { unResMap :: H.HashMap i r }

class (Hashable i, Eq i, Applicative m) => Resource i r m | i -> r m where
        loadResource :: i -> m r
        unloadResource :: i -> r -> m ()

newResMap :: Resource i r m => ResMap i r
newResMap = ResMap H.empty

addResource :: Resource i r m => i -> ResMap i r -> m (ResMap i r)
addResource i m = snd <$> getResource i m

getResource :: Resource i r m => i -> ResMap i r -> m (r, ResMap i r)
getResource i (ResMap map) = case H.lookup i map of
                                Just r -> pure (r, ResMap map)
                                Nothing -> fmap (\r ->
                                                 (r, ResMap $ H.insert i r map))
                                                (loadResource i)

removeResource :: Resource i r m => i -> ResMap i r -> m (ResMap i r)
removeResource i (ResMap map) = unloadResource i (map H.! i)
                                $> ResMap (H.delete i map)
