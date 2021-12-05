module Day4 where

testDraws :: [Integer]
testDraws = [7, 4, 9, 5, 11, 17, 23, 2, 0, 14, 21, 24, 10, 16, 13, 6, 15, 25, 12, 22, 18, 20, 8, 19, 3, 26, 1]

-- testDraws :: [Integer]
-- testDraws = [22, 13, 17, 11, 0, 44]

testBoardA :: [[Integer]]
testBoardA =
  [ [22, 13, 17, 11, 0],
  [8, 2, 23, 4, 24],
  [21, 9, 14, 16, 7],
  [6, 10, 3, 18, 5],
  [1, 12, 20, 15, 19]
  ]

testGame :: [[[Integer]]]
testGame = [testBoardA]

prepareScorePerBoard :: [[Integer]] -> [[(Integer, Bool)]]
prepareScorePerBoard = map (map (\element -> (element, False)))

drawNumber :: Integer -> [[(Integer, Bool)]] -> [[(Integer, Bool)]]
drawNumber number = map (map (\(element, current) -> (element, current || (number == element))))

checkRowsForWin :: [[(Integer, Bool)]] -> Bool
checkRowsForWin = any (all (\(_, isPicked) -> isPicked))

runGamePerBoard :: [[(Integer, Bool)]] -> [Integer] -> Bool
runGamePerBoard scoreboard [] = False
runGamePerBoard scoreboard (curDraw : rest) = checkRowsForWin (drawNumber curDraw scoreboard) || runGamePerBoard (drawNumber curDraw scoreboard) rest

debugGame :: [[[(Integer, Bool)]]]
debugGame = scanr (\curDraw acc -> (drawNumber curDraw acc)) (prepareScorePerBoard testBoardA) testDraws