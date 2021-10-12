module Funcy where

import System.Directory.Internal.Prelude (Integral)

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

-- Guards
avgGrade :: (Ord a, Fractional a) => a -> Char
avgGrade x
  | y >= 0.9 = 'A'
  | y >= 0.8 = 'B'
  | y >= 0.7 = 'C'
  | y >= 0.59 = 'D'
  | y < 0.59 = 'F'
  where
    y = x / 100

numbers :: (Ord a, Num a, Num p) => a -> p
numbers x
  | x < 0 = -1
  | x == 0 = 0
  | x > 0 = 1

-- chapter exercises

tensDigit :: Integral a => a -> a
tensDigit x = d
  where
    xLast = x `div` 10
    d = xLast `mod` 10

tensDigitSol :: Integral a => a -> a
tensDigitSol = f
  where
    dm y = divMod y 10
    -- d = snd (dm (fst (dm x)))
    f = snd . dm . fst . dm