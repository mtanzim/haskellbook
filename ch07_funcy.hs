module Funcy where

-- Anon functions
fnIfOdd :: Integer -> (Integer -> Integer) -> Integer
fnIfOdd = \n -> \f -> case odd n of
  True -> f n
  False -> n

addFive :: Integer -> Integer -> Integer
addFive = \x -> \y -> (if x > y then y else x) + 5

mflip :: (t1 -> t2 -> t3) -> t2 -> t1 -> t3
mflip f x y = f y x

-- Pattern matching
f ::
  (a, b, c) ->
  (d, e, f) ->
  ((a, d), (c, f))
f (a, b, c) (d, e, f) = ((a, d), (c, f))

funcC :: Bool -> Bool -> Bool
funcC x y =
  case x > y of
    True -> x
    _ -> False

ifEvenAdd2 :: Integral p => p -> p
ifEvenAdd2 n =
  case even n of
    True -> n + 2
    _ -> n

nums :: (Ord a, Num a, Num p) => a -> p
nums x =
  case compare x 0 of
    LT -> -1
    GT -> 1
    EQ -> 0

-- HOF
dodgy :: Num a => a -> a -> a
dodgy x y = x + y * 10

oneIsOne :: Integer -> Integer
oneIsOne = dodgy 1

oneIsTwo :: Integer -> Integer
oneIsTwo = (flip dodgy) 2
