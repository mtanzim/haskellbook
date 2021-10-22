module Cipher where

import Data.Char (chr, ord)

common :: (Int -> Int -> Int) -> Int -> [Char] -> [Char]
common op n = map (chr . fn)
  where
    fn x = (+) 97 (mod ((-) (sumVal x) 97) 26)
    sumVal x = op (ord x) (mod n 26)

caesar :: Int -> [Char] -> [Char]
caesar = common (+)

unCaesar :: Int -> [Char] -> [Char]
unCaesar = common (-)

isValid :: Bool
isValid = unCaesar 1 (caesar 1 "abracadbra") == "abracadbra"