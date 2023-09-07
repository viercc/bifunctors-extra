{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Data.Bifunctor.Monad.Module where

import Data.Bifunctor
import Data.Bifoldable (Bifoldable)
import Data.Bitraversable (Bitraversable (..))
import Data.Functor.Identity
import Data.Proxy
import Data.Profunctor

class (Monad m, forall a. Monad (p a), Bifunctor p) => HalfModule m p | p -> m where
    -- |
    --
    -- * 'firstAction' is an action of monad @m@
    --
    --   @
    --   firstAction pure = id
    --   firstAction f . firstAction g = firstAction (f <=< g)
    --   @
    --
    -- * 'firstAction f' is a monad homomorphism
    --
    --   @
    --   firstAction f (pure b) = pure b
    --   firstAction f (pab >>= k) = firstAction f pab >>= (firstAction f . k)
    --   @
    firstAction :: (a -> m a') -> p a b -> p a' b

newtype NoAction p a b = NoAction { runNoAction :: p a b }
    deriving stock (Eq, Ord, Show, Read)
    deriving newtype
      (Functor, Foldable, Applicative, Monad,
       Bifunctor, Bifoldable)

instance Traversable (p a) => Traversable (NoAction p a) where
    traverse f = fmap NoAction . traverse f . runNoAction

instance Bitraversable p => Bitraversable (NoAction p) where
    bitraverse f g = fmap NoAction . bitraverse f g . runNoAction

instance (Bifunctor p, forall a. Monad (p a)) => HalfModule Identity (NoAction p) where
    firstAction f = first (runIdentity . f)

newtype IgnoreAction p a b = IgnoreAction { runIgnoreAction :: p a b }
    deriving stock (Eq, Ord, Show, Read)
    deriving newtype
      (Functor, Foldable, Applicative, Monad,
       Bifunctor, Bifoldable,
       Profunctor)

instance Traversable (p a) => Traversable (IgnoreAction p a) where
    traverse f = fmap IgnoreAction . traverse f . runIgnoreAction

instance Bitraversable p => Bitraversable (IgnoreAction p) where
    bitraverse f g = fmap IgnoreAction . bitraverse f g . runIgnoreAction

instance (Bifunctor p, Profunctor p, forall a. Monad (p a)) => HalfModule Proxy (IgnoreAction p) where
    firstAction = ignoreAction

phantomFirst :: (Bifunctor p, Profunctor p) => p a b -> p a' b
phantomFirst = lmap (const ()) . first (const ())

ignoreAction :: (Bifunctor p, Profunctor p) => (a -> m a') -> p a b -> p a' b
ignoreAction _ = phantomFirst