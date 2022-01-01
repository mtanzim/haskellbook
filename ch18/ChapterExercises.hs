module ChapterExercises where

import Test.QuickCheck
import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

data Nope a = NopeDotJpg deriving (Eq, Show)

instance Functor Nope where
  fmap _ NopeDotJpg = NopeDotJpg

instance Applicative Nope where
  pure _ = NopeDotJpg
  _ <*> _ = NopeDotJpg

instance Monad Nope where
  return = pure
  _ >>= _ = NopeDotJpg

instance Arbitrary (Nope a) where
  arbitrary = elements [NopeDotJpg]

instance (Eq a) => EqProp (Nope a) where
  (=-=) = eq

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

instance Monoid b => Monad (BahEither b) where
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

newtype Identity a = Identity a deriving (Eq, Show)

instance Functor Identity where
  fmap f (Identity a) = Identity (f a)

instance Applicative Identity where
  pure a = Identity a
  Identity f <*> Identity a = Identity (f a)

instance Monad Identity where
  return = pure
  Identity a >>= f = f a

identityGen :: Arbitrary a => Gen (Identity a)
identityGen = do
  fmap Identity arbitrary

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = identityGen

instance (Eq a) => EqProp (Identity a) where
  (=-=) = eq

test trigger = do
  quickBatch $ functor trigger
  quickBatch $ applicative trigger
  quickBatch $ monad trigger

main :: IO ()
main = do
  putStrLn ("\nBahEither")
  test (undefined :: BahEither String (Int, Int, Int))
  putStrLn ("\nNope")
  test (undefined :: Nope (String, String, String))
  putStrLn ("\nIdentity")
  test (undefined :: Identity (String, String, String))