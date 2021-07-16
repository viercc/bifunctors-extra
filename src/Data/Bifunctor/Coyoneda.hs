{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE StandaloneDeriving #-}
module Data.Bifunctor.Coyoneda where

import Data.Bifunctor
import Data.Bifoldable
import Data.Semigroup.Bifoldable

data Coyoneda p a b = forall a' b'. Coyoneda (a' -> a) (b' -> b) (p a' b')

liftCoyoneda :: p a b -> Coyoneda p a b
liftCoyoneda = Coyoneda id id

lowerCoyoneda :: Bifunctor p => Coyoneda p a b -> p a b
lowerCoyoneda (Coyoneda f g p) = bimap f g p

deriving instance Functor (Coyoneda p a)

instance Bifunctor (Coyoneda p) where
  bimap f g (Coyoneda f' g' p0) = Coyoneda (f . f') (g . g') p0
  first f (Coyoneda f' g' p0) = Coyoneda (f . f') g' p0
  second = fmap

instance Bifoldable p => Bifoldable (Coyoneda p) where
  bifoldMap f g (Coyoneda f' g' p0) = bifoldMap (f . f') (g . g') p0

instance Bifoldable1 p => Bifoldable1 (Coyoneda p) where
  bifoldMap1 f g (Coyoneda f' g' p0) = bifoldMap1 (f . f') (g . g') p0
