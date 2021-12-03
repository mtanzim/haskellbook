module AdventDay3 where

import Data.Char (digitToInt)
import Data.List (foldl')

testInput :: [String]
testInput = ["00100", "11110", "10110"]

numCharsPerEntry :: Int
numCharsPerEntry = length (head testInput)

curCol :: Int -> Int -> [[a]] -> [a]
curCol curIdx maxIdx input = if curIdx > maxIdx then [] else map (!! curIdx) input

gatherColumns :: [[a]] -> [[a]]
gatherColumns input = map (\b -> curCol b limit input) [0 .. limit]
  where
    limit = numCharsPerEntry - 1

countZeros :: String -> Int
countZeros = length . filter (== '0')

countOnes :: String -> Int
countOnes = length . filter (== '1')

gammaBits :: [String] -> String
gammaBits = map (\lst -> if countZeros lst > countOnes lst then '0' else '1') . gatherColumns

epsilonBits :: [String] -> String
epsilonBits = map (\lst -> if countZeros lst > countOnes lst then '1' else '0') . gatherColumns

-- stolen from: https://stackoverflow.com/a/26961027
binCharToDec :: String -> Int
binCharToDec = foldl' (\acc x -> acc * 2 + digitToInt x) 0

day3Input :: IO [String]
day3Input = do
  binaryEntries <- readFile "day3Input.txt"
  return (lines binaryEntries)

-- main = (binCharToDec (gammaBits testInput) * binCharToDec (epsilonBits testInput))

main :: IO ()
main = do
  input <- day3Input
  print (binCharToDec (gammaBits input) * binCharToDec (epsilonBits input))