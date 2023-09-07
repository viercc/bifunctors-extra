{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE QuantifiedConstraints #-}
-- | 'Control.Monad.Free' but taking a secondary covariant parameter.
module Data.Bifunctor.Free where

import Control.Monad.Free (MonadFree)
import qualified Control.Monad.Free as Original
import Data.Bifunctor
import Data.Bifunctor.Functor

newtype Free p a b = WrapFree { unwrapFree :: Original.Free (p a) b }
   deriving (Functor, Applicative, Monad)

instance Functor (p a) => MonadFree (p a) (Free p a) where
  wrap = WrapFree . Original.Free . fmap unwrapFree

instance Bifunctor p => Bifunctor (Free p) where
    bimap f g = WrapFree . go . unwrapFree
      where
        go (Original.Pure b) = Original.Pure (g b)
        go (Original.Free pam) = Original.Free (bimap f go pam)
    
    first f = WrapFree . Original.hoistFree (first f) . unwrapFree
    second = fmap

hoistFree :: Bifunctor q => (p :-> q) -> (Free p :-> Free q)
hoistFree h = WrapFree . Original.hoistFree h . unwrapFree