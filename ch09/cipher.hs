module Cipher where

import Data.Char (chr, ord)

caesar :: Int -> [Char] -> [Char]
caesar n = map (chr . fn)
  where
    sumVal x = (+) (mod n 26) (ord x)
    fn x = (+) 97 (mod ((-) (sumVal x) 97) 26)