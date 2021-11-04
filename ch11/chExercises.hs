module Ch11Exercises where

import Data.Char (chr, ord, toUpper)
import qualified Data.Text.Internal.Fusion.Common as Char

-- TODO: name these functions better
caesar :: Int -> Char -> Char
caesar = fn (+)
  where
    fn op n = chr . fn'
      where
        fn' x = (+) 97 (mod ((-) (sumVal x) 97) 26)
        sumVal x = op (ord x) (mod n 26)

-- TODO: spaces do not work
cipherVignere :: String -> String -> String
cipherVignere xs ks =
  go xs 0
  where
    go xs' i =
      case xs' of
        [head] -> [caesar curShiftVal head]
        (head : tail) -> caesar curShiftVal head : go tail (i + 1)
        _ -> []
      where
        curShiftVal = shiftVal curChar
        curChar = ks !! curI
        curI = mod i (length ks)
        shiftVal c = ord c - 97

isSubseqOf :: (Eq a) => [a] -> [a] -> Bool
isSubseqOf xs@(x : _) ys = elem x ys && isSubseqOf (tail xs) ys
isSubseqOf [] _ = True

capitalizeWords :: String -> [(String, String)]

capitalizeWords = fn . words
  where
    fn wsArr =
      case wsArr of
        [] -> []
        (word@(firstWord : rest)) : tail -> ((toUpper firstWord : rest), word) : fn tail
