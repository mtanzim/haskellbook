module ListApplicative where

import Test.QuickCheck
import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

data List a = Nil | Cons a (List a) deriving (Eq, Show)

instance Functor List where
  fmap _ Nil = Nil
  fmap f (Cons a lst) = Cons (f a) (fmap f lst)

append :: List a -> List a -> List a
append Nil ys = ys
append (Cons x xs) ys = Cons x (xs `append` ys)

instance Applicative List where
  pure a = Cons a Nil
  Nil <*> _ = Nil
  _ <*> Nil = Nil
  (Cons f fs) <*> xs = append (fmap f xs) (fs <*> xs)

listGen :: Arbitrary a => Gen (List a)
listGen = do
  a <- arbitrary
  lst <- listGen
  frequency [(1, return Nil), (1, return (Cons a lst))]

instance Arbitrary a => Arbitrary (List a) where
  arbitrary = listGen

instance Eq a => EqProp (List a) where
  (=-=) = eq

main :: IO ()
main = quickBatch $ applicative (Cons ("a", "b", "c") Nil)