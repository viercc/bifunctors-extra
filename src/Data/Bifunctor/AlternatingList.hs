{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE BangPatterns #-}
module Data.Bifunctor.AlternatingList(
  AltList(..),
  head, tail,
  
  separate, foldMapBoth,
  toNonEmptyBy,
  
  glue, flatMap,
  reverse, unfoldr, unfoldl, unfoldTree,
  
  fromEithers,fromEithers',

  bizip, bizipWith, birepeat,
  unzip
) where

import Prelude hiding (head, tail, unzip, reverse)

import Control.Monad (ap)
import Data.Bifoldable
import Data.Bifunctor
import Data.Bitraversable
import Data.Functor.Apply
import Data.Semigroup.Bifoldable
import Data.Semigroup.Bitraversable
import Data.Semigroup (Semigroup(..))
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import Data.List (genericReplicate)
import Data.Maybe (fromMaybe)
import Data.Semigroup.Foldable
import Data.Function (fix)

data AltList a b = Last a | Next a b (AltList a b)
  deriving (Show, Read, Eq, Ord, Functor, Foldable, Traversable)

head :: AltList a b -> a
head (Last a) = a
head (Next a _ _) = a

tail :: AltList a b -> Maybe (b, AltList a b)
tail (Last _) = Nothing
tail (Next _ b r) = Just (b, r)

separate :: AltList a b -> (NonEmpty a, [b])
separate = first toNE . go
  where
    toNE = fromMaybe (error "Impossible!") . NE.nonEmpty
    go (Last a) = ([a], [])
    go (Next a b r) = case go r of
      ~(as,bs) -> (a:as, b:bs)

foldMapBoth :: (Semigroup ra, Monoid rb) => (a -> ra) -> (b -> rb) -> AltList a b -> (ra, rb)
foldMapBoth f g = bimap (foldMap1 f) (foldMap g) . separate

-- @toNonEmpty f g = 'bifoldMap' ('NE.singleton' . f) (singleton . g)@
toNonEmptyBy :: (a -> c) -> (b -> c) -> AltList a b -> NonEmpty c
toNonEmptyBy f g = go
  where
    go (Last a) = NE.singleton (f a)
    go (Next a b r) = case go r of
      ~(c :| cs) -> f a :| g b : c : cs

-- TODO : documentation

-- There is another 'Monad'-like structure on AltList here,
-- defined using
--
-- > pure = Last
-- > xs >>= f = flatMap f xs

-- | @glue xs b ys@
--
-- > xs = [ax, bx, ax, bx, ..., ax]
-- > ys = [ay, by, ay, by, ..., ay]
-- > glue xs b0 ys = [ax, bx, ax, bx, ..., ax, b0, ay, by, ..., by, ay]
glue :: AltList a b -> b -> AltList a b -> AltList a b
glue (Last a) b suffix = Next a b suffix
glue (Next a1 b1 r) b suffix = Next a1 b1 (glue r b suffix)

flatMap :: (a -> AltList b c) -> AltList a c -> AltList b c
flatMap f = go
  where
    go (Last a) = f a
    go (Next a c r) = glue (f a) c (go r)

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

unfoldTree :: (s -> Either a (s,b,s)) -> s -> AltList a b
unfoldTree step s0 = go s0 Last
  where
    go s cont = case step s of
      Left a -> cont a
      Right (l, b, r) -> go l (\a -> Next a b (go r cont))

fromEithers :: (Monoid a) => [Either a b] -> AltList a b
fromEithers = foldr (\x r -> either Last pure x <> r) (Last mempty)

fromEithers' :: (Monoid a) => [Either a b] -> AltList a b
fromEithers' = go mempty
  where
    go !acc [] = Last acc
    go !acc (Left a : xs) = go (acc <> a) xs
    go !acc (Right b : xs) = Next acc b (go mempty xs)

instance (Semigroup a) => Semigroup (AltList a b) where
  Last a <> Last a' = Last (a <> a')
  Last a <> Next a' b r = Next (a <> a') b r
  Next a b r <> ab' = Next a b (r <> ab')

  stimes n
     | n <  1 = error "stimes: positive multiplier expected"
     | n == 1 = id
     | otherwise = aux
    where
      -- Special case for @length xs == 0@
      aux (Last a) = Last (stimes n a)
      -- Special case for @length xs == 1@
      aux (Next a0 b (Last a1)) = Next a0 b $ foldr (\_ -> Next a' b) (Last a1) (genericReplicate (n-1) ())
        where a' = a1 <> a0
      aux xs = stimesReplicate n xs

stimesReplicate :: (Integral b, Semigroup x) => b -> x -> x
stimesReplicate b x = foldr1 (<>) (genericReplicate b x)

instance (Monoid a) => Monoid (AltList a b) where
  mempty = Last mempty

instance (Monoid a) => Applicative (AltList a) where
  pure b = Next mempty b (Last mempty)

  xs <*> Last a' = Last $ bifoldMap id (const a') xs
  xs <*> ys = ap xs ys

  xs *> ys = id <$ xs <*> ys

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

-- | (TODO: documentation)
--
--   AltList could be an instance of 'Data.Biapplicative' using 'bizipWith' as 'Data.Biapplicative.biliftA2',
--   but that instance is not provided here.
--
--   This is because to avoid confusion between 'Applicative' instance, which works like
--   how 'Applicative []' instance work.
bizipWith :: (a -> a' -> a'') -> (b -> b' -> b'') -> AltList a b -> AltList a' b' -> AltList a'' b''
bizipWith f g = go where
  go (Next a b r) (Next a' b' r') = Next (f a a') (g b b') (go r r')
  go ab ab' = Last (f (head ab) (head ab'))

bizip :: AltList a b -> AltList a' b' -> AltList (a,a') (b,b')
bizip = bizipWith (,) (,)

birepeat :: a -> b -> AltList a b
birepeat a b = fix (Next a b)

unzip :: AltList (a,a') (b,b') -> (AltList a b, AltList a' b')
unzip (Last (a,a')) = (Last a, Last a')
unzip (Next (a,a') (b,b') r) = case unzip r of
  ~(x, x') -> (Next a b x, Next a' b' x')
