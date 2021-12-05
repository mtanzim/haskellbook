module Day4 where

testDraws :: [Integer]
testDraws = [7, 4, 9, 5, 11, 17, 23, 2, 0, 14, 21, 24, 10, 16, 13, 6, 15, 25, 12, 22, 18, 20, 8, 19, 3, 26, 1]

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
testGame = [testBoardA, testBoardB, testBoardC]

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
runGame boards lastMarkedBoards (curDraw : rest) =
  let currentMarkedBoards = (map (drawNumber curDraw) lastMarkedBoards)
      currentWinners = filter (\markedBoard -> checkRowsForWin markedBoard || checkRowsForWin (transposeBoard markedBoard)) currentMarkedBoards
      winningGame = head currentWinners
   in case currentWinners of
        [winningBoard : _] -> Just (curDraw * countScoreFromUnMarked winningGame)
        [] -> runGame boards (map (drawNumber curDraw) currentMarkedBoards) rest
        _ -> Nothing

main :: Maybe Integer
main = runGame testGame (map prepareScorePerBoard testGame) testDraws

debugGame :: [MarkedGameBoard]
debugGame = scanr (\curDraw acc -> (drawNumber curDraw acc)) (prepareScorePerBoard testBoardB) testDraws