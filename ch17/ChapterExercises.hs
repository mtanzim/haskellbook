module ChapterExercises where

import Test.QuickCheck
import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

data Pair a = Pair a a deriving (Eq, Show)

-- instance Semigroup a => Semigroup (Pair a) where
--   Pair x x' <> Pair y y' = Pair (x <> x') (y <> y')

-- instance Monoid a => Monoid (Pair a) where
--   mappend = (<>)
--   mempty = Pair mempty mempty

instance Functor Pair where
  fmap f (Pair a a') = Pair (f a) (f a')

instance Applicative Pair where
  pure a = Pair a a
  Pair f f' <*> Pair x x' = Pair (f x) (f' x')

pairGen :: Arbitrary a => Gen (Pair a)
pairGen = do
  a <- arbitrary
  return (Pair a a)

instance Arbitrary a => Arbitrary (Pair a) where
  arbitrary = pairGen

instance Eq a => EqProp (Pair a) where
  (=-=) = eq

data Two a b = Two a b deriving (Eq, Show)

instance Functor (Two a) where
  fmap f (Two a b) = Two a (f b)

instance Monoid a => Applicative (Two a) where
  pure b = Two mempty b
  (Two a fb) <*> (Two a' b) = Two (a <> a') (fb b)

twoGen :: (Arbitrary a, Arbitrary b) => Gen (Two a b)
twoGen = do
  a <- arbitrary
  b <- arbitrary
  return (Two a b)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = twoGen

instance (Eq a, Eq b) => EqProp (Two a b) where
  (=-=) = eq

main :: IO ()
main = do
  quickBatch $ applicative (Pair ('a', 'b', 'c') ('d', 'e', 'f'))
  quickBatch $ applicative (Two ("abc", "bbc", "cbc") ("abc", "bbc", "cbc"))