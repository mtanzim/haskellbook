module Day5 where

import qualified Data.Map as Map

type Coordinate = (Integer, Integer)

type LineDefinition = (Coordinate, Coordinate)

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

buildCoordinateMap :: Map.Map Coordinate Integer -> [Coordinate] -> Map.Map Coordinate Integer
buildCoordinateMap currentMap (head : tail) =
  let currentValue = (Map.findWithDefault 0 head currentMap) + 1
   in buildCoordinateMap (Map.insert head currentValue currentMap) tail
buildCoordinateMap currentMap [] = currentMap

filterIntersectingPoints :: Map.Map Coordinate Integer -> Map.Map Coordinate Integer
filterIntersectingPoints = Map.filter (> 1)

numOverlappingPointsFromStraightLines :: [LineDefinition] -> Int
numOverlappingPointsFromStraightLines =
  Map.size
    . filterIntersectingPoints
    . buildCoordinateMap Map.empty
    . concat
    . collectPointsFromStraightLines
    . filterStraightLines

testInput :: [LineDefinition]
testInput =
  [ ((0, 9), (5, 9)),
    ((5, 9), (5, 8)),
    ((0, 9), (0, 5))
  ]

testMain :: Int
testMain = numOverlappingPointsFromStraightLines testInput