module AdventDay3 where

import Data.Char (digitToInt)
import Data.List (foldl')

testInput :: [String]
testInput = ["00100", "11110", "10110"]

testLimit :: Int
testLimit = length (head testInput) - 1

curCol :: Int -> Int -> [[a]] -> [a]
curCol curIdx maxIdx input = if curIdx > maxIdx then [] else map (!! curIdx) input

gatherColumns :: Int -> [[a]] -> [[a]]
gatherColumns limit input = map (\b -> curCol b limit input) [0 .. limit]

countZeros :: String -> Int
countZeros = length . filter (== '0')

countOnes :: String -> Int
countOnes = length . filter (== '1')

gammaBits :: Int -> [String] -> String
gammaBits limit = map (\lst -> if countZeros lst > countOnes lst then '0' else '1') . gatherColumns limit

epsilonBits :: Int -> [String] -> String
epsilonBits limit = map (\lst -> if countZeros lst > countOnes lst then '1' else '0') . gatherColumns limit

oxygenRating :: [String] -> Int -> Int -> [String] -> String
oxygenRating [head] _ _ _ = head
oxygenRating lst curIdx limit input = oxygenRating (getFiltered limit lst) (curIdx + 1) limit input
  where
    getFiltered limit' curLst = filter (\row -> (gammaBits limit' curLst !! curIdx) == (row !! curIdx)) curLst

co2Rating :: [String] -> Int -> Int -> [String] -> String
co2Rating [head] _ _ _ = head
co2Rating lst curIdx limit input = co2Rating (getFiltered limit lst) (curIdx + 1) limit input
  where
    getFiltered limit' curLst = filter (\row -> (epsilonBits limit' curLst !! curIdx) == (row !! curIdx)) curLst

-- stolen from: https://stackoverflow.com/a/26961027
binCharToDec :: String -> Int
binCharToDec = foldl' (\acc x -> acc * 2 + digitToInt x) 0

day3Input :: IO [String]
day3Input = do
  binaryEntries <- readFile "day3Input.txt"
  return (lines binaryEntries)

day3Limit :: IO Int
day3Limit = do
  entries <- day3Input
  return (length (head entries) - 1)

main :: IO ()
main = do
  input <- day3Input
  limit <- day3Limit
  print (binCharToDec (gammaBits limit input) * binCharToDec (epsilonBits limit input))
  print (lifeSupportRating input limit)
  where
    lifeSupportRating input limit = binCharToDec (oxygenRating input 0 limit input) * binCharToDec (co2Rating input 0 limit input)
