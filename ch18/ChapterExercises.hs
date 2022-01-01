module ChapterExercises where

import Test.QuickCheck
import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

data BahEither b a = PRight b | PLeft a deriving (Eq, Show)

instance Functor (BahEither b) where
  fmap _ (PRight b) = PRight b
  fmap f (PLeft a) = PLeft (f a)

instance Monoid b => Applicative (BahEither b) where
  pure a = PLeft a
  PRight b <*> PRight b' = PRight (b <> b')
  PRight b <*> _ = PRight b
  _ <*> PRight b = PRight b
  PLeft f <*> PLeft a = PLeft (f a)

instance Monoid a => Monad (BahEither a) where
  return = pure
  (PRight b) >>= _ = PRight b
  (PLeft a) >>= f = f a

genEither :: (Arbitrary a, Arbitrary b) => Gen (BahEither b a)
genEither = do
  a <- arbitrary
  b <- arbitrary
  elements [PLeft a, PRight b]

instance (Arbitrary a, Arbitrary b) => Arbitrary (BahEither b a) where
  arbitrary = genEither

instance (Eq a, Eq b) => EqProp (BahEither b a) where
  (=-=) = eq

test trigger = do
  quickBatch $ functor trigger
  quickBatch $ applicative trigger
  quickBatch $ monad trigger

main = do
  test (undefined :: BahEither String (Int, Int, Int))