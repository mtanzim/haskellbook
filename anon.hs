module Anon where

fnIfOdd :: Integer -> (Integer -> Integer) -> Integer
fnIfOdd = \n -> \f -> case odd n of
  True -> f n
  False -> n

addFive :: Integer -> Integer -> Integer
addFive = \x -> \y -> (if x > y then y else x) + 5

mflip :: (t1 -> t2 -> t3) -> t2 -> t1 -> t3
mflip f x y = f y x