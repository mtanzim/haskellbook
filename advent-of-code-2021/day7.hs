module Day7 where

import Data.List
import Data.List.Split

day7Input :: IO [Integer]
day7Input = do
  inputs <- readFile "day7Input.txt"
  return (map (\x -> read x :: Integer) (splitOn "," inputs))

main :: IO ()
main = do
  input <- day7Input
  let sortedInput = sort input
   in print (gatherRelativeDistances sortedInput 0 (0, sum sortedInput) sum)

gatherRelativeDistances :: [Integer] -> Int -> (Integer, Integer) -> ([Integer] -> Integer) -> (Integer, Integer)
gatherRelativeDistances lst curIdx (minPos, minTotalDistance) summingFn =
  if curIdx == length lst - 1
    then (minPos, minTotalDistance)
    else
      let curVal = lst !! curIdx
          relativeDistances = map (\x -> abs (curVal - x)) lst
          totalDisplacement = summingFn relativeDistances
          latestMinDistance = min totalDisplacement minTotalDistance
          latestMinPos = if latestMinDistance < minTotalDistance then curVal else minPos
       in gatherRelativeDistances lst (curIdx + 1) (latestMinPos, latestMinDistance) summingFn

testInput :: [Integer]
testInput = [16, 1, 2, 0, 4, 2, 7, 1, 2, 14]

testMain :: (Integer, Integer)
testMain =
  let sortedInput = sort testInput
   in gatherRelativeDistances sortedInput 0 (0, sum sortedInput) sum