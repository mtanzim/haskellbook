module ChapterExercises where

import Test.QuickCheck
import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

newtype Identity a = Identity a deriving (Eq, Ord, Show)

instance Functor Identity where
  fmap f (Identity a) = Identity (f a)

instance Applicative Identity where
  pure = Identity
  Identity f <*> Identity a = Identity (f a)

instance Foldable Identity where
  foldMap f (Identity a) = f a

instance Traversable Identity where
  traverse f (Identity a) = Identity <$> f a

identityGen :: Arbitrary a => Gen (Identity a)
identityGen = do
  a <- arbitrary
  return (Identity a)

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = identityGen

instance (Eq a) => EqProp (Identity a) where
  (=-=) = eq

newtype Constant a b = Constant {getConstant :: a} deriving (Eq, Show)

instance Functor (Constant a) where
  fmap _ (Constant a) = Constant a

instance Monoid a => Applicative (Constant a) where
  pure _ = Constant mempty
  (Constant a) <*> (Constant a') = Constant (a <> a')

instance Foldable (Constant a) where
  foldMap _ _ = mempty

instance Traversable (Constant a) where
  traverse f (Constant a) = pure (Constant a)

constantGen :: (Arbitrary a, Arbitrary b) => Gen (Constant a b)
constantGen = do
  a <- arbitrary
  return (Constant a)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Constant a b) where
  arbitrary = constantGen

instance (Eq a, Eq b) => EqProp (Constant a b) where
  (=-=) = eq

test trigger = do
  quickBatch $ traversable trigger

type Trigger = (Int, [Int], [Char])

main :: IO ()
main = do
  let trigger = undefined
  test (trigger :: Identity Trigger)
  test (trigger :: Constant Int Trigger)