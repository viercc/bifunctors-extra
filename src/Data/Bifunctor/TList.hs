{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE TupleSections #-}
module Data.Bifunctor.TList where

import Prelude hiding (concatMap, last, foldr)
import Data.Bifoldable
import Data.Bifunctor
import Data.Bitraversable
import Data.Functor.Apply
import Data.Semigroup.Foldable
import Data.Semigroup.Bifoldable
import Data.Semigroup.Bitraversable
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import qualified Data.List as List

data TList a b = End b | Cons a (TList a b)
    deriving (Show, Read, Eq, Ord, Functor, Traversable)

last :: TList a b -> b
last (End b) = b
last (Cons _ as) = last as

prepend, (++>) :: [a] -> TList a b -> TList a b
prepend xs y = List.foldr Cons y xs
(++>) = prepend

infixr 5 ++>

foldr :: (a -> r -> r) -> (b -> r) -> TList a b -> r
foldr f g = go
  where
    go (End b) = g b
    go (Cons a x) = f a (go x)

unfoldr :: (s -> Either b (a, s)) -> s -> TList a b
unfoldr step = go
  where
    go s = case step s of
        Left b -> End b
        Right (a,s') -> Cons a (go s')

toList :: TList a b -> ([a], b)
toList = foldr (\ a ~(as, b) -> (a:as, b)) ([], )

toList_ :: TList a b -> [a]
toList_ = foldr (:) (const [])

fromList :: [a] -> TList a ()
fromList = List.foldr Cons (End ())

fromListWithLength :: [a] -> TList a Int
fromListWithLength = mapAccumL (\n a -> (, a) $! succ n) 0

toNonEmpty :: TList a a -> NonEmpty a
toNonEmpty (End a) = a :| []
toNonEmpty (Cons a x) = a :| foldr (:) (: []) x

fromNonEmpty :: NonEmpty a -> TList a a
fromNonEmpty as = case NE.uncons as of
    (a, Nothing) -> End a
    (a, Just as') -> Cons a (fromNonEmpty as')

concatMap :: (a -> [a']) -> TList a b -> TList a' b
concatMap f = go
  where
    go (End b) = End b
    go (Cons a x) = f a ++> go x

filter :: (a -> Bool) -> TList a b -> TList a b
filter p = concatMap (\a -> List.filter p [a])

-- | Infinite-list-safe 'Data.List.mapAccumL'
mapAccumL :: (s -> a -> (s, b)) -> s -> [a] -> TList b s
mapAccumL step = go
  where
    go s [] = End s
    go s (a:as) = case step s a of
        (s', b) -> Cons b (go s' as)

-- | Infinite-list-safe 'Data.List.mapAccumR'
mapAccumR :: (s -> a -> (s, b)) -> TList a s -> (s, [b])
mapAccumR step = foldr step' (, [])
  where
    step' a ~(s, bs) = case step s a of
        (s', b) -> (s', b:bs)

instance Bifunctor TList where
    bimap f g = go
      where
        go (End b) = End (g b)
        go (Cons a x) = Cons (f a) (go x)

instance Foldable (TList a) where
    foldMap f = f . last
    null = const False
    length = const 1

instance Foldable1 (TList a) where
    foldMap1 f = f . last

instance Bifoldable TList where
    bifoldMap = bifoldMapDefault
    
    bifoldr f g z = foldr f (\b -> g b z)

instance Bifoldable1 TList where
    bifoldMap1 = bifoldMap1Default

instance Bitraversable TList where
    bitraverse f g = go
      where
        go (End b) = End <$> g b
        go (Cons a x) = Cons <$> f a <*> go x

instance Bitraversable1 TList where
    bitraverse1 f g = go
      where
        go (End b) = End <$> g b
        go (Cons a x) = Cons <$> f a <.> go x

instance Applicative (TList a) where
    pure = End
    End x <*> y = x <$> y
    Cons a x <*> y = Cons a (x <*> y)

    End x <* y = x <$ y
    Cons a x <* y = Cons a (x <* y)
    
    End _ *> y = y
    Cons a x *> y = Cons a (x *> y)

instance Monad (TList a) where
    End x >>= k = k x
    Cons a x >>= k = Cons a (x >>= k)