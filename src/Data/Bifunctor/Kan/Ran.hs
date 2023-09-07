{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DeriveFunctor #-}
module Data.Bifunctor.Kan.Ran where

import Data.Bifunctor

newtype Ran p1 p2 q c d = Ran2 {
    runRan :: forall a b. (c -> p1 a b) -> (d -> p2 a b) -> q a b
  }
  deriving Functor

instance Bifunctor (Ran p1 p2 q) where
    bimap f g r = Ran2 $ \k1 k2 -> runRan r (k1 . f) (k2 . g)
