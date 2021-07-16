{-# LANGUAGE DeriveTraversable #-}

module Data.Bifunctor.AlternatingList(
    AltList(..),
    head, foldMapBoth,toListOfPairs) where

import Prelude hiding (head)

import Control.Monad (ap)
import Data.Bifoldable
import Data.Bifunctor
import Data.Bitraversable
import Data.Functor.Apply
import Data.Semigroup.Bifoldable
import Data.Semigroup.Bitraversable
import Data.Semigroup (Semigroup(stimes))
import Data.List (genericReplicate)

data AltList a b = Last a | Next a b (AltList a b)
  deriving (Show, Read, Eq, Ord, Functor, Foldable, Traversable)

head :: AltList a b -> a
head (Last a) = a
head (Next a _ _) = a

toListOfPairs :: AltList a b -> (a, [(b,a)])
toListOfPairs = go
  where
    go (Last a) = (a, [])
    go (Next a b r) = case go r of
      ~(a0,baList) -> (a, (b, a0) : baList)

foldMapBoth :: (Semigroup ra, Monoid rb) => (a -> ra) -> (b -> rb) -> AltList a b -> (ra, rb)
foldMapBoth f g = go
  where
    go (Last a) = (f a, mempty)
    go (Next a b r) = case go r of
      ~(ra, rb) -> (f a <> ra, g b <> rb)

instance (Monoid a) => Semigroup (AltList a b) where
  Last a <> Last a' = Last (a <> a')
  Last a <> Next a' b r = Next (a <> a') b r
  Next a b r <> ab' = Next a b (r <> ab')

  stimes n
     | n <= 0    = aux
     | otherwise = error "stimes: positive multiplier expected"
    where
      aux (Last a) = Last (stimes n a)
      aux (Next a0 b (Last a1)) = Next a0 b $ foldr (\_ -> Next a' b) (Last a1) (genericReplicate (n-1) ())
        where a' = a1 <> a0
      aux xs = foldr1 (<>) (genericReplicate n xs)

instance (Monoid a) => Monoid (AltList a b) where
  mempty = Last mempty

instance (Monoid a) => Applicative (AltList a) where
  pure b = Next mempty b (Last mempty)
  Last a <*> _ = Last a
  xs <*> Last a' = Last $ bifoldMap id (const a') xs
  xs <*> ys = ap xs ys

instance (Monoid a) => Monad (AltList a) where
  xs >>= k = bifoldMap Last k xs
  (>>) = (*>)

instance Bifunctor AltList where
  first f (Last a) = Last (f a)
  first f (Next a b r) = Next (f a) b (first f r)

  second = fmap

  bimap = bimapDefault

instance Bifoldable AltList where
  bifoldMap = bifoldMapDefault

instance Bifoldable1 AltList where
  bifoldMap1 = bifoldMap1Default

instance Bitraversable AltList where
  bitraverse f g = go
    where
      go (Last a) = Last <$> f a
      go (Next a b r) = Next <$> f a <*> g b <*> go r

instance Bitraversable1 AltList where
  bitraverse1 f g = go
    where
      go (Last a) = Last <$> f a
      go (Next a b r) = Next <$> f a <.> g b <.> go r