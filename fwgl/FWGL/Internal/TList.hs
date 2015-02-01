{-# LANGUAGE DataKinds, KindSignatures, MultiParamTypeClasses, TypeFamilies,
             TypeOperators, UndecidableInstances, FlexibleContexts,
             FlexibleInstances, ConstraintKinds, PolyKinds #-}

module FWGL.Internal.TList where

class Subset (xs :: [*]) (ys :: [*])
instance Subset '[] ys
instance (Subset '[x] ys, Subset xs ys) => Subset (x ': xs) ys
-- BUG: Subset x x?

-- class Equal (xs :: [*]) (ys :: [*])
-- instance (Subset xs ys, Subset ys xs) => Equal xs ys
type Equal xs ys = (Subset xs ys, Subset ys xs)

type Member x xs = Subset '[x] xs

type NotMember x xs = IsMember x xs ~ False

type family IsMember x (xs :: [*]) :: Bool where
        IsMember x '[] = False
        IsMember x (x ': xs) = True
        IsMember y (x ': xs) = IsMember y xs

type family Remove x (xs :: [*]) where
        Remove x '[] = '[]
        Remove x (x ': xs) = Remove x xs
        Remove x (y ': xs) = y ': Remove x xs

type family Difference (xs :: [*]) (ys :: [*]) where
        Difference xs '[] = xs
        Difference xs (y ': ys) = Difference (Remove y xs) ys

type family Append (xs :: [*]) (ys :: [*]) where
        Append '[] ys = ys
        Append (x ': xs) ys = x ': Append xs ys

type family Insert y (xs :: [*]) where
        Insert y '[] = '[y]
        Insert y (y ': xs) = y ': xs
        Insert y (x ': xs) = x ': Insert y xs

type family Union (xs :: [*]) (ys :: [*]) where
        Union '[] ys = ys
        Union (x ': xs) ys = Union xs (Insert x ys)

type family TypeEq (x :: k) (y :: k) :: Bool where
        TypeEq x x = True
        TypeEq x y = False
