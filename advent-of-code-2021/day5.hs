module Day5 where

import Data.List.Split
import qualified Data.Map as Map

type Coordinate = (Integer, Integer)

type LineDefinition = (Coordinate, Coordinate)

isLineStraight :: LineDefinition -> Bool
isLineStraight ((x1, y1), (x2, y2)) = x1 == x2 || y1 == y2

filterStraightLines :: [LineDefinition] -> [LineDefinition]
filterStraightLines = filter isLineStraight

filterDiagonalLines :: [LineDefinition] -> [LineDefinition]
filterDiagonalLines = filter (not . isLineStraight)

collectPointsFromStraightLines :: [LineDefinition] -> [[Coordinate]]
collectPointsFromStraightLines = map fn
  where
    fn ((x1, y1), (x2, y2)) =
      let maxX = max x1 x2
          minX = min x1 x2
          maxY = max y1 y2
          minY = min y1 y2
          xRange = [minX .. maxX]
          yRange = [minY .. maxY]
       in [(x', y') | x' <- xRange, y' <- yRange]

collectPointsFromDiagonalLines :: [LineDefinition] -> [[Coordinate]]
collectPointsFromDiagonalLines = map fn
  where
    fn ((x1, y1), (x2, y2)) =
      let xRange = if x1 < x2 then [x1 .. x2] else reverse [x2 .. x1]
          yRange = if y1 < y2 then [y1 .. y2] else reverse [y2 .. y1]
       in zip xRange yRange

buildCoordinateMap :: Map.Map Coordinate Integer -> [Coordinate] -> Map.Map Coordinate Integer
buildCoordinateMap currentMap (head : tail) =
  let currentValue = (Map.findWithDefault 0 head currentMap) + 1
   in buildCoordinateMap (Map.insert head currentValue currentMap) tail
buildCoordinateMap currentMap [] = currentMap

filterIntersectingPoints :: Map.Map Coordinate Integer -> Map.Map Coordinate Integer
filterIntersectingPoints = Map.filter (> 1)

collectPointsFromAllLines input = (concat (collectPointsFromDiagonalLines (filterDiagonalLines input))) ++ (concat (collectPointsFromStraightLines (filterStraightLines input)))

numOverlappingPointsFromStraightLines :: [LineDefinition] -> Int
numOverlappingPointsFromStraightLines =
  Map.size
    . filterIntersectingPoints
    . buildCoordinateMap Map.empty
    . concat
    . collectPointsFromStraightLines
    . filterStraightLines

numOverlappingPointsFromAllLines :: [LineDefinition] -> Int
numOverlappingPointsFromAllLines =
  Map.size
    . filterIntersectingPoints
    . buildCoordinateMap Map.empty
    . collectPointsFromAllLines

testInput :: [LineDefinition]
testInput =
  [ ((0, 9), (5, 9)),
    ((5, 9), (5, 8)),
    ((0, 9), (0, 5)),
    ((1, 1), (3, 3)),
    ((9, 7), (7, 9))
  ]

testMain :: Int
testMain = numOverlappingPointsFromStraightLines testInput

day5Input :: IO [LineDefinition]
day5Input = do
  inputs <- readFile "day5Input.txt"

  let parseLines = ((map (splitOn " -> ")) . lines)
      parsePoints = map (map (splitOn ","))
      convertPointToInteger = map (map (map (\x -> read x :: Integer)))
      arrToTupleOuter = map (map (\lines -> (lines !! 0, lines !! 1)))
      arrToTupleInner = map (\line -> (line !! 0, line !! 1))
   in return ((arrToTupleInner . arrToTupleOuter . convertPointToInteger . parsePoints . parseLines) inputs)

main :: IO ()
main = do
  input <- day5Input
  print (numOverlappingPointsFromStraightLines input)
  print (numOverlappingPointsFromAllLines input)
