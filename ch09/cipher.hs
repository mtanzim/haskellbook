module Cipher where

import Data.Char (chr, ord)

caesar :: Int -> [Char] -> [Char]
caesar n = map (chr . fn)
  where
    fn x = (+) 97 (mod ((-) (sumVal x) 97) 26)
    sumVal x = (+) (ord x) (mod n 26) 


unCaesar :: Int -> [Char] -> [Char]
unCaesar n = map (chr . fn)
  where
    fn x = (+) 97 (mod ((-) (sumVal x) 97) 26)
    sumVal x = (-) (ord x) (mod n 26)