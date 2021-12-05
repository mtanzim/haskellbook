module Day4 where

testDraws :: [Integer]
testDraws = [7, 4, 9, 5, 11, 17, 23, 2, 0, 14, 21, 24, 10, 16, 13, 6, 15, 25, 12, 22, 18, 20, 8, 19, 3, 26, 1]

-- testDraws :: [Integer]
-- testDraws = [22, 13, 17, 11, 0, 44]

type GameBoard = [[Integer]]

type MarkedGameBoard = [[(Integer, Bool)]]

testBoardB :: GameBoard
testBoardB =
  [ [3, 15, 0, 2, 22],
    [9, 18, 13, 17, 5],
    [19, 8, 7, 25, 23],
    [20, 11, 10, 24, 4],
    [14, 21, 16, 12, 6]
  ]

testBoardC :: GameBoard
testBoardC =
  [ [14, 21, 17, 24, 4],
    [10, 16, 15, 9, 19],
    [18, 8, 23, 26, 20],
    [22, 11, 13, 6, 5],
    [2, 0, 12, 3, 7]
  ]

testGame :: [GameBoard]
testGame = [testBoardB, testBoardC]

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

runGame :: [GameBoard] -> [MarkedGameBoard] -> [Integer] -> Maybe Integer
runGame _ _ [] = Nothing
runGame boards latestMarkedBoards (curDraw : rest) =
  let currentMarkedBoards = (map (drawNumber curDraw) latestMarkedBoards)
      currentWinners = filter (\markedBoard -> checkRowsForWin markedBoard || checkRowsForWin (transposeBoard markedBoard)) currentMarkedBoards
      winningGame = head currentWinners
   in if length currentWinners > 0
        then Just (curDraw * countScoreFromUnMarked winningGame)
        else runGame boards (map (drawNumber curDraw) currentMarkedBoards) rest

main :: Maybe Integer
main = runGame testGame (map prepareScorePerBoard testGame) testDraws

debugGame :: [MarkedGameBoard]
debugGame = scanr (\curDraw acc -> (drawNumber curDraw acc)) (prepareScorePerBoard testBoardB) testDraws