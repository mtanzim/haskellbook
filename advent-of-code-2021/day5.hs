module Day5 where

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

testMain :: [Coordinate]
testMain = (concat . collectPointsFromStraightLines . filterStraightLines) testInput