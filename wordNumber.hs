module WordNumber where

import Data.List (intersperse)

digitToWord :: Int -> String
digitToWord n = undefined

digits :: Int -> [Int]
digits n = go n []
  where
    go n' lst
      | n' == 0 = lst
      | otherwise = go d (m:lst)
      where
        (d, m) = divMod n' 10

wordNumber :: Int -> String
wordNumber n = undefined