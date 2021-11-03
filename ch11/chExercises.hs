module Ch11Exercises where

import Data.Char (chr, ord)

shiftVal :: Char -> Int
shiftVal c = ord c - 97

common :: (Int -> Int -> Int) -> Int -> Char -> Char
common op n = chr . fn
  where
    fn x = (+) 97 (mod ((-) (sumVal x) 97) 26)
    sumVal x = op (ord x) (mod n 26)

caesar :: Int -> Char -> Char
caesar = common (+)

cipherVignere :: String -> String -> String
cipherVignere xs ks =
  go xs 0
  where
    go xs' i =
      case xs' of
        (head : []) -> (caesar (shiftVal (ks !! curI)) head) : []
        (head : tail) -> (caesar (shiftVal (ks !! curI)) head) : go tail (i + 1)
        _ -> []
      where
        curI = mod i (length ks)