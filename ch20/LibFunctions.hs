module LibFunctions where

import Data.Foldable
import Data.Monoid

sum' :: (Foldable t, Num a) => t a -> a
sum' t = getSum $ foldMap Sum t