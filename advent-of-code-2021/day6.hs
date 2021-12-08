module Day6 where

import Data.List.Split
import qualified Data.Map as Map

day6Input :: IO [Integer]
day6Input = do
  inputs <- readFile "day6Input.txt"
  return (map (\x -> read x :: Integer) (splitOn "," inputs))

main :: IO ()
main = do
  input <- day6Input
  print (fishSimulator input 80)

testInput :: [Integer]
testInput = [3, 4, 3, 1, 2]

elapse :: [Integer] -> [Integer]
elapse currentFishes =
  let curZeroes = countZeroes currentFishes
      fishesToAdd = map (const 8) [1 .. curZeroes]
   in map (\x -> if x == 0 then 6 else x -1) currentFishes ++ fishesToAdd

countZeroes :: [Integer] -> Integer
countZeroes = foldr (\x acc -> if x == 0 then acc + 1 else acc) 0

fishSimulator :: [Integer] -> Integer -> Int
fishSimulator initState days = length (foldr (\day acc -> elapse acc) initState [1 .. days])

buildStatusMap :: [Integer] -> Map.Map Integer Integer
buildStatusMap = foldr (\x curMap -> Map.insert x (newValue curMap x) curMap) Map.empty
  where
    newValue curMap x = (Map.findWithDefault 0 x curMap) + 1

elapseFaster :: Map.Map Integer Integer -> Map.Map Integer Integer
elapseFaster currentFishMap =
  let curZeroes = Map.findWithDefault 0 0 currentFishMap
   in Map.fromList
        [ (0, Map.findWithDefault 0 1 currentFishMap),
          (1, Map.findWithDefault 0 2 currentFishMap),
          (2, Map.findWithDefault 0 3 currentFishMap),
          (3, Map.findWithDefault 0 4 currentFishMap),
          (4, Map.findWithDefault 0 5 currentFishMap),
          (5, Map.findWithDefault 0 6 currentFishMap),
          (6, Map.findWithDefault 0 7 currentFishMap + Map.findWithDefault 0 0 currentFishMap),
          (7, Map.findWithDefault 0 8 currentFishMap),
          (8, Map.findWithDefault 0 6 currentFishMap + curZeroes)
        ]

fishSimulatorFaster :: [Integer] -> Integer -> Map.Map Integer Integer
fishSimulatorFaster initState days = foldr (\day acc -> elapseFaster acc) (buildStatusMap initState) [1 .. days]

testMain = fishSimulatorFaster testInput 3