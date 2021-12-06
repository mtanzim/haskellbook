module Day5 where

import qualified Data.Map as Map

type Coordinate = (Integer, Integer)

type LineDefinition = (Coordinate, Coordinate)

testInput :: [LineDefinition]
testInput =
  [ ((0, 9), (5, 9)),
    ((8, 0), (0, 8)),
    ((7, 0), (7, 4))
  ]

filterStraightLines :: [LineDefinition] -> [LineDefinition]
filterStraightLines = filter fn
  where
    fn ((x1, y1), (x2, y2)) = x1 == x2 || y1 == y2

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

buildOccurences :: Map.Map Coordinate Integer -> [Coordinate] -> Map.Map Coordinate Integer
buildOccurences currentMap (head : tail) =
  let currentValue = (Map.findWithDefault (-1) head currentMap) + 1
   in buildOccurences (Map.insert head currentValue currentMap) tail
buildOccurences currentMap [] = currentMap

testMain :: Map.Map Coordinate Integer
testMain = ((buildOccurences Map.empty) . concat . collectPointsFromStraightLines . filterStraightLines) testInput