module Day7(day7Main) where

import Data.List
import Data.List.Split

day7Input :: IO [Integer]
day7Input = do
  inputs <- readFile "day7Input.txt"
  return (map (\x -> read x :: Integer) (splitOn "," inputs))

day7Main :: IO ()
day7Main = do
  input <- day7Input
  let sortedInput = sort input
      range = [head sortedInput .. last sortedInput]
      maxPossibleConst = sum range
      maxPossibleWithAccel = sumWithAcceleration range
   in do
        -- TODO: can be better than O(n^2)? Memoization for improvements?
        print (getMinDisplacement sortedInput range 0 (0, maxPossibleConst) sum)
        print (getMinDisplacement sortedInput range 0 (0, maxPossibleWithAccel) sumWithAcceleration)

-- https://en.wikipedia.org/wiki/Triangular_number
sumWithAcceleration :: [Integer] -> Integer
sumWithAcceleration = sum . map (\x -> (x * (x + 1)) `div` 2)

getMinDisplacement :: [Integer] -> [Integer] -> Int -> (Integer, Integer) -> ([Integer] -> Integer) -> (Integer, Integer)
getMinDisplacement lst range curIdx (minPos, minTotalDistance) summingFn =
  if curIdx == length range - 1
    then (minPos, minTotalDistance)
    else
      let curVal = range !! curIdx
          relativeDistances = map (\x -> abs (curVal - x)) lst
          totalDisplacement = summingFn relativeDistances
          latestMinDistance = min totalDisplacement minTotalDistance
          latestMinPos = if latestMinDistance < minTotalDistance then curVal else minPos
       in getMinDisplacement lst range (curIdx + 1) (latestMinPos, latestMinDistance) summingFn

-- DEBUG

testInput :: [Integer]
testInput = [16, 1, 2, 0, 4, 2, 7, 1, 2, 14]

testMain :: (Integer, Integer)
testMain =
  let sortedInput = sort testInput
      range = [head sortedInput .. last sortedInput]
   in getMinDisplacement sortedInput range 0 (0, sumWithAcceleration range) sumWithAcceleration