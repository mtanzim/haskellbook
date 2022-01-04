module SkiFree where

import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

data S n a = S (n a) a deriving (Eq, Show)

instance (Functor n, Arbitrary (n a), Arbitrary a) => Arbitrary (S n a) where
  arbitrary = S <$> arbitrary <*> arbitrary

instance (Applicative n, Testable (n Property), Eq a, Eq (n a), EqProp a) => EqProp (S n a) where
  (=-=) = eq

-- TODO: revisit this, solution doesn't really make sense?
-- https://github.com/lisss/yaths/blob/f0ffa728fe1e167b0744a8c3ba538949475ae0b2/src/ch20-21/Traversable.hs#L98
-- I thought we left the first kind argument alone?
-- Although given the Tree instances the recursive nature is similar
instance Functor n => Functor (S n) where
  fmap f (S na a) = S (fmap f na) (f a)

instance Foldable n => Foldable (S n) where
  foldMap f (S na a) = foldMap f na <> f a

instance Traversable n => Traversable (S n) where
  traverse f (S na a) = S <$> traverse f na <*> f a

type Trigger = ([Int], [Int], [Char])

main :: IO ()
main = do
--   sample' (arbitrary :: Gen (S [] Int))
  quickBatch $ traversable (undefined :: S [] Trigger)
