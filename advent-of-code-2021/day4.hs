module Day4 where

testDraws :: [Integer]
testDraws = [7, 4, 9, 5, 11, 17, 23, 2, 0, 14, 21, 24, 10, 16, 13, 6, 15, 25, 12, 22, 18, 20, 8, 19, 3, 26, 1]

-- testDraws :: [Integer]
-- testDraws = [22, 13, 17, 11, 0, 44]

type GameBoard = [[Integer]]

type MarkedGameBoard = [[(Integer, Bool)]]

testBoardA :: GameBoard
testBoardA =
  [ [14, 21, 17, 24, 4],
    [10, 16, 15, 9, 19],
    [18, 8, 23, 26, 20],
    [22, 11, 13, 6, 5],
    [2, 0, 12, 3, 7]
  ]

testGame :: [GameBoard]
testGame = [testBoardA]

prepareScorePerBoard :: GameBoard -> MarkedGameBoard
prepareScorePerBoard = map (map (\element -> (element, False)))

drawNumber :: Integer -> MarkedGameBoard -> MarkedGameBoard
drawNumber number = map (map (\(element, current) -> (element, current || (number == element))))

checkRowsForWin :: MarkedGameBoard -> Bool
checkRowsForWin = any (all (\(_, isPicked) -> isPicked))

transposeBoard :: MarkedGameBoard -> MarkedGameBoard
transposeBoard scoreboard =
  let boardLength = length (head scoreboard) - 1
   in map (\curIdx -> map (!! curIdx) scoreboard) [0 .. boardLength]

runGamePerBoard :: MarkedGameBoard -> [Integer] -> Integer
runGamePerBoard markedBoard [] = -1
runGamePerBoard markedBoard (curDraw : rest) =
  let latestMarkedBoard = drawNumber curDraw markedBoard
   in if checkRowsForWin latestMarkedBoard || checkRowsForWin (transposeBoard latestMarkedBoard)
        then curDraw * countScoreFromUnMarked latestMarkedBoard
        else runGamePerBoard (drawNumber curDraw markedBoard) rest

countScoreFromUnMarked :: MarkedGameBoard -> Integer
countScoreFromUnMarked = sum . map fst . filter (not . snd) . concat

debugGame :: [MarkedGameBoard]
debugGame = scanr (\curDraw acc -> (drawNumber curDraw acc)) (prepareScorePerBoard testBoardA) testDraws