module Exercises where

import Control.Applicative
import Data.List (elemIndex)

f :: (Eq a, Num a) => a -> Maybe [Char]
f x = lookup x [(3, "hello"), (4, "julie"), (5, "kbai")]

g :: (Eq a, Num a) => a -> Maybe [Char]
g x = lookup x [(7, "sup?"), (8, "chris"), (9, "aloha")]

h :: (Eq a, Num a, Num b) => a -> Maybe b
h z = lookup z [(2, 3), (5, 6), (7, 8)]

m :: (Eq a, Num a, Num b) => a -> Maybe b
m x = lookup x [(4, 10), (8, 13), (1, 9001)]

added :: Maybe Integer
added = fmap (+ 3) (lookup 3 $ zip [1, 2, 3] [4, 5, 6])

y :: Maybe Integer
y = lookup 3 $ zip [1, 2, 3] [4, 5, 6]

z :: Maybe Integer
z = lookup 2 $ zip [1, 2, 3] [4, 5, 6]

tupled :: Maybe (Integer, Integer)
tupled = (,) <$> y <*> z

x :: Maybe Int
x = elemIndex 3 [1, 2, 3, 4, 5]

y' :: Maybe Int
y' = elemIndex 4 [1, 2, 3, 4, 5]

max' :: Int -> Int -> Int
max' = max

maxed :: Maybe Int
maxed = max' <$> x <*> y'

xs :: [Integer]
xs = [1, 3, 3]

ys :: [Integer]
ys = [4, 5, 6]

x3 :: Maybe Integer
x3 = lookup 3 $ zip xs ys

y2 :: Maybe Integer
y2 = lookup 3 $ zip xs ys

summed :: Maybe Integer
summed = fmap sum ((,) <$> x3 <*> y2)

newtype Identity a = Identity a deriving (Eq, Ord, Show)

instance Functor Identity where
  fmap f (Identity a) = Identity (f a)

instance Applicative Identity where
  pure = Identity
  Identity f <*> Identity a = Identity (f a)

newtype Constant a b = Constant {getConstant :: a}
  deriving (Eq, Ord, Show)

instance Functor (Constant a) where
  fmap _ (Constant a) = Constant a

instance Monoid a => Applicative (Constant a) where
  pure _ = Constant mempty
  (Constant a) <*> (Constant a') = Constant (a <> a')

-- fixer upper
fixerUpper1 :: Maybe [Char]
fixerUpper1 = const <$> Just "Hello" <*> Just "World"

fixerUpper2 :: Maybe (Integer, Integer, [Char], [Integer])
fixerUpper2 = (,,,) <$> Just 90 <*> Just 10 <*> Just "Tierness" <*> Just [1, 2, 3, 4]