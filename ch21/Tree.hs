module Tree where

import Test.QuickCheck
import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

data Tree a = Empty | Leaf a | Node (Tree a) (Tree a) deriving (Eq, Show)

instance Functor Tree where
  fmap _ Empty = Empty
  fmap f (Leaf a) = Leaf (f a)
  fmap f (Node t1 t2) = Node (fmap f t1) (fmap f t2)

instance Foldable Tree where
  foldMap f Empty = mempty
  foldMap f (Leaf a) = f a
  foldMap f (Node t1 t2) = foldMap f t1 <> foldMap f t2

instance Traversable Tree where
  traverse f Empty = pure Empty
  traverse f (Leaf a) = Leaf <$> f a
  traverse f (Node t1 t2) = Node <$> traverse f t1 <*> traverse f t2

treeGen :: Arbitrary a => Gen (Tree a)
treeGen = do
  t1 <- treeGen
  t2 <- treeGen
  a <- arbitrary
  elements
    [ Leaf a,
      Empty,
      Node t1 t2
    ]

instance Arbitrary a => Arbitrary (Tree a) where
  arbitrary = treeGen

instance Eq a => EqProp (Tree a) where
  (=-=) = eq

test trigger = do
  quickBatch $ traversable trigger

type Trigger = ([Int], [Int], [Char])

main :: IO ()
main = do
  let trigger = undefined
  test (trigger :: Tree Trigger)