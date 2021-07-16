{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DeriveFunctor #-}
module Data.Bifunctor.Yoneda where

import Data.Bifunctor
import Data.Bifoldable
import Data.Semigroup.Bifoldable

newtype Yoneda p a b = Yoneda {
        runYoneda :: forall a' b'. (a -> a') -> (b -> b') -> p a' b'
    }
    deriving Functor

liftYoneda :: Bifunctor p => p a b -> Yoneda p a b
liftYoneda p = Yoneda (\f g -> bimap f g p)

lowerYoneda :: Yoneda p a b -> p a b
lowerYoneda (Yoneda k) = k id id

instance Bifunctor (Yoneda p) where
    bimap f g (Yoneda k) = Yoneda (\f' g' -> k (f' . f) (g'. g))
    first f (Yoneda k) = Yoneda (\f' g -> k (f' . f) g)
    second = fmap

instance Bifoldable p => Bifoldable (Yoneda p) where
    bifoldMap f g (Yoneda k) = bifold $ k f g

instance Bifoldable1 p => Bifoldable1 (Yoneda p) where
    bifoldMap1 f g (Yoneda k) = bifold1 $ k f g
