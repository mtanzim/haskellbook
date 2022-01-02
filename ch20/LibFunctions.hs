module LibFunctions where

import Data.Foldable
import Data.Monoid

sum' :: (Foldable t, Num a) => t a -> a
sum' = getSum . foldMap Sum

product' :: (Foldable t, Num a) => t a -> a
product' = getProduct . foldMap Product

elem' :: (Foldable t, Eq a) => a -> t a -> Bool
elem' a t = getAny $ foldMap (Any . (== a)) t

minimum' :: (Foldable t, Ord a) => t a -> Maybe a
minimum' = foldr fn Nothing
  where
    fn cur acc = case (acc, cur) of
      (Nothing, cur') -> Just cur'
      (Just acc', cur') -> if cur' < acc' then Just cur' else Just acc'

maximum' :: (Foldable t, Ord a) => t a -> Maybe a
maximum' = foldr fn Nothing
  where
    fn cur acc = case (acc, cur) of
      (Nothing, cur') -> Just cur'
      (Just acc', cur') -> if cur' > acc' then Just cur' else Just acc'

length' :: (Foldable t) => t a -> Int
length' = foldr (\acc cur -> cur + 1) 0

null' :: (Foldable t) => t a -> Bool
null' = (== 0) . length'

toList' :: (Foldable t) => t a -> [a]
toList' = foldMap (: [])

fold' :: (Foldable t, Monoid m) => t m -> m
fold' = foldMap id

foldMap' :: (Foldable t, Monoid m) => (a -> m) -> t a -> m
foldMap' f = foldr (\cur acc -> f cur <> acc) mempty