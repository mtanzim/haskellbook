module WordNumber where

import Data.List (intersperse)

digitToWord :: Int -> String
digitToWord n =
  case n of
    0 -> "zero"
    1 -> "one"
    2 -> "two"
    3 -> "three"
    4 -> "four"
    5 -> "five"
    6 -> "six"
    7 -> "seven"
    8 -> "eight"
    9 -> "nine"
    _ -> "messed up bro"

digits :: Int -> [Int]
digits n = go n []
  where
    go n' lst
      | n' == 0 = lst
      | otherwise = go d (m : lst)
      where
        (d, m) = divMod n' 10

wordNumber :: Int -> String
wordNumber n = concat (intersperse "-" (map digitToWord (digits n)))