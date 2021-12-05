module Day4 where

testDraws :: [Integer]
testDraws = [7, 4, 9, 5, 11, 17, 23, 2, 0, 14, 21, 24, 10, 16, 13, 6, 15, 25, 12, 22, 18, 20, 8, 19, 3, 26, 1]

-- testDraws :: [Integer]
-- testDraws = [22, 13, 17, 11, 0, 44]

type GameBoard = [[Integer]]

type MarkedGameBoard = [[(Integer, Bool)]]

testBoardA :: GameBoard
testBoardA =
  [ [22, 13, 17, 11, 0],
    [8, 2, 23, 4, 24],
    [21, 9, 14, 16, 7],
    [6, 10, 3, 18, 5],
    [1, 12, 20, 15, 19]
  ]

testGame :: [GameBoard]
testGame = [testBoardA]

prepareScorePerBoard :: GameBoard -> MarkedGameBoard
prepareScorePerBoard = map (map (\element -> (element, False)))

drawNumber :: Integer -> MarkedGameBoard -> MarkedGameBoard
drawNumber number = map (map (\(element, current) -> (element, current || (number == element))))

checkRowsForWin :: MarkedGameBoard -> Bool
checkRowsForWin = any (all (\(_, isPicked) -> isPicked))

runGamePerBoard :: MarkedGameBoard -> [Integer] -> (Bool, Integer, MarkedGameBoard)
runGamePerBoard scoreboard [] = (False, -1, scoreboard)
runGamePerBoard scoreboard (curDraw : rest) =
  let latestScoreboard = drawNumber curDraw scoreboard
   in if checkRowsForWin latestScoreboard then (True, curDraw, latestScoreboard) else runGamePerBoard (drawNumber curDraw scoreboard) rest

debugGame :: [MarkedGameBoard]
debugGame = scanr (\curDraw acc -> (drawNumber curDraw acc)) (prepareScorePerBoard testBoardA) testDraws