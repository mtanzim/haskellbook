module Cipher where

import Data.Char (chr, ord)

caesar :: Int -> [Char] -> [Char]
caesar n xs = map chr (map fn xs)
  where
    sumVal x = (+) (ord x) (mod n 26)
    fn x = (mod ((sumVal x) - 97) 26) + 97