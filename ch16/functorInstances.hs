module FunctorInstances where

import Test.QuickCheck

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

identityGen :: Arbitrary a => Gen (Identity a)
identityGen = do
  fmap Identity arbitrary

main :: IO ()
main = do
  quickCheck (f :: Identity Int -> Bool)
  quickCheck li
  where
    f x = functorIdentity x
    c = functorCompose (+ 1) (* 2)
    li x = c (x :: Identity Int)