module LibFunctions where

import Data.Foldable
import Data.Monoid

sum' :: (Foldable t, Num a) => t a -> a
sum' = getSum . foldMap Sum

product' :: (Foldable t, Num a) => t a -> a
product' = getProduct . foldMap Product

elem' :: (Foldable t, Eq a) => a -> t a -> Bool
elem' a t = getAny $ foldMap (Any . (== a)) t

-- TODO: agh come back to this
-- minimum' :: (Foldable t, Ord a) => t a -> Maybe a
-- minimum' = foldr fn Nothing
--   where
--     fn acc cur = case (acc, cur) of
--       (Nothing, cur') -> Just cur'
--       (Just acc', cur') -> if cur' < acc' then Just cur' else Just acc'

-- maximum' = (Foldable t, Ord a) => t a -> Maybe a
