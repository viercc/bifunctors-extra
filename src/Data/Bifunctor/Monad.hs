{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE DefaultSignatures #-}
module Data.Bifunctor.Monad where

import Data.Bifunctor

import Data.Bifunctor.Clown
import Data.Bifunctor.Joker
import Data.Coerce (coerce)

class Bifunctor p => Monad1 p where
    pureFirst :: a -> p a b
    bindFirst :: p a b -> (a -> p a' b) -> p a' b

class Bifunctor q => Monad2 q where
    pureSecond :: b -> q a b
    default pureSecond :: Monad (q a) => b -> q a b
    pureSecond = pure
    
    bindSecond :: q a b -> (b -> q a b') -> q a b'
    default bindSecond :: Monad (q a) => q a b -> (b -> q a b') -> q a b'
    bindSecond = (>>=)

class (Monad1 p, Monad2 q) => PairedMonad p q where
    cross1 :: p a b -> (a -> p a' b') -> (b -> q a' b') -> p a' b'
    cross2 :: q a b -> (a -> p a' b') -> (b -> q a' b') -> q a' b'

instance Monad f => Monad1 (Clown f) where
    pureFirst :: forall a b. a -> Clown f a b
    pureFirst = coerce (pure @f @a)
    bindFirst :: forall a b a'. Clown f a b -> (a -> Clown f a' b) -> Clown f a' b
    bindFirst = coerce ((>>=) @f @a @a')

instance Monad f => Monad2 (Joker f) where
    pureSecond :: forall a b. b -> Joker f a b
    pureSecond = coerce (pure @f @b)
    bindSecond :: forall a b b'. Joker f a b -> (b -> Joker f a b') -> Joker f a b'
    bindSecond = coerce ((>>=) @f @b @b')

instance (Monad f, Monad g) => PairedMonad (Clown f) (Joker g) where
    cross1 (Clown p) f _ = Clown (p >>= runClown . f)
    cross2 (Joker q) _ g = Joker (q >>= runJoker . g)