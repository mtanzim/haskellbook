module FunctorInstances where

import Test.QuickCheck
import Test.QuickCheck (Arbitrary)

-- Instances of Functor

functorIdentity :: (Functor f, Eq (f a)) => f a -> Bool
functorIdentity f = fmap id f == f

functorCompose :: (Functor f, Eq (f c)) => (a -> b) -> (b -> c) -> f a -> Bool
functorCompose f g x = (fmap g (fmap f x)) == (fmap (g . f) x)

newtype Identity a = Identity a deriving (Eq, Show)

instance Functor Identity where
  fmap f (Identity a) = Identity (f a)

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = identityGen

instance Arbitrary a => Arbitrary (Pair a) where
  arbitrary = pairGen

pairGen :: Arbitrary a => Gen (Pair a)
pairGen = do
  a <- arbitrary
  return (Pair a a)

identityGen :: Arbitrary a => Gen (Identity a)
identityGen = do
  fmap Identity arbitrary

data Pair a = Pair a a deriving (Eq, Show)

instance Functor Pair where
  fmap f (Pair a a') = Pair (f a) (f a')

main :: IO ()
main = do
  quickCheck f
  quickCheck li
  quickCheck fPair
  where
    f :: Identity Int -> Bool
    fPair :: Pair Int -> Bool
    f x = functorIdentity x
    fPair x = functorIdentity x
    c = functorCompose (+ 1) (* 2)
    li x = c (x :: Identity Int)
    cPair = functorCompose (+ 1) (* 2)
    liPair x = cPair (x :: Pair Int)
