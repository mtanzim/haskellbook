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

main :: IO ()
main = do
    quickBatch $ applicative (Pair ('a', 'b', 'c') ('d', 'e', 'f'))