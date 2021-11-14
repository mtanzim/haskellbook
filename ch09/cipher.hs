module Cipher where

import Data.Char (chr, ord)
import System.IO

common :: (Int -> Int -> Int) -> Int -> [Char] -> [Char]
common op n = map (chr . fn)
  where
    fn x = (+) 97 (mod ((-) (sumVal x) 97) 26)
    sumVal x = op (ord x) (mod n 26)

caesar :: Int -> [Char] -> [Char]
caesar = common (+)

unCaesar :: Int -> [Char] -> [Char]
unCaesar = common (-)

isValid :: Int -> Bool
isValid n = unCaesar n (caesar n ['a' .. 'z']) == ['a' .. 'z']

areAllValid :: Bool
areAllValid = notElem False (map isValid [1 .. 52])

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  putStr "Please type in the word to caesar: "
  word <- getLine
  putStr "How many to shift by: "
  shift <- getLine
  putStrLn (caesar (read shift :: Int) word)
