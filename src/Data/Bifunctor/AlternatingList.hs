{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE BangPatterns #-}

module Data.Bifunctor.AlternatingList(
  AltList(..),
  head, unzip, foldMapBoth,
  toListOfPairs, 
  
  reverse, unfoldr, unfoldl, fromEithers,fromEithers'
) where

import Prelude hiding (head, unzip, reverse)

import Control.Monad (ap)
import Data.Bifoldable
import Data.Bifunctor
import Data.Bitraversable
import Data.Functor.Apply
import Data.Semigroup.Bifoldable
import Data.Semigroup.Bitraversable
import Data.Semigroup (Semigroup(stimes))
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import Data.List (genericReplicate)
import Data.Maybe (fromMaybe)
import Data.Semigroup.Foldable

data AltList a b = Last a | Next a b (AltList a b)
  deriving (Show, Read, Eq, Ord, Functor, Foldable, Traversable)

head :: AltList a b -> a
head (Last a) = a
head (Next a _ _) = a

unzip :: AltList a b -> (NonEmpty a, [b])
unzip = first toNE . go
  where
    toNE = fromMaybe (error "Impossible!") . NE.nonEmpty
    go (Last a) = ([a], [])
    go (Next a b r) = case go r of
      ~(as,bs) -> (a:as, b:bs)

foldMapBoth :: (Semigroup ra, Monoid rb) => (a -> ra) -> (b -> rb) -> AltList a b -> (ra, rb)
foldMapBoth f g = bimap (foldMap1 f) (foldMap g) . unzip

toListOfPairs :: AltList a b -> (a, [(b,a)])
toListOfPairs = go
  where
    go (Last a) = (a, [])
    go (Next a b r) = case go r of
      ~(a0,baList) -> (a, (b, a0) : baList)

reverse :: AltList a b -> AltList a b
reverse (Last a) = Last a
reverse (Next a0 b0 r0) = go (Last a0) b0 r0
  where
    go s b (Last a) = Next a b s
    go s b (Next a b' r) = go (Next a b s) b' r

unfoldr :: (s -> Either a (a,b,s)) -> s -> AltList a b
unfoldr step = go
  where
    go s = either Last (\(a,b,s') -> Next a b (go s')) $ step s

unfoldl :: (s -> Either a (s,b,a)) -> s -> AltList a b
unfoldl step = reverse . unfoldr step'
  where
    step' s = case step s of
      Left a -> Left a
      Right (s',b,a) -> Right (a,b,s')

fromEithers :: (Monoid a) => [Either a b] -> AltList a b
fromEithers = foldr (\x r -> either Last pure x <> r) (Last mempty)

fromEithers' :: (Monoid a) => [Either a b] -> AltList a b
fromEithers' = go mempty
  where
    go !acc [] = Last acc
    go !acc (Left a : xs) = go (acc <> a) xs
    go !acc (Right b : xs) = Next acc b (go mempty xs)

instance (Monoid a) => Semigroup (AltList a b) where
  Last a <> Last a' = Last (a <> a')
  Last a <> Next a' b r = Next (a <> a') b r
  Next a b r <> ab' = Next a b (r <> ab')

  stimes n
     | n <= 0    = error "stimes: positive multiplier expected"
     | otherwise = aux
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