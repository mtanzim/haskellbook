module Day4 where

import Data.List.Split

type GameBoard = [[Integer]]

type MarkedGameBoard = [[(Integer, Bool)]]

day4Draws :: IO [Integer]
day4Draws = do
  draws <- readFile "day4Draws.txt"
  return (map (\x -> read x :: Integer) (splitOn "," draws))

day4Boards :: IO [GameBoard]
day4Boards = do
  boards <- readFile "day4Boards.txt"
  let chunks = chunksOf 5 (filter (/= "") (lines boards))
      chunks' = map (\chunk -> (map (\line -> words line) chunk)) chunks
      gameBoards = map (\chunk -> (map (map (\x -> read x :: Integer)) chunk)) chunks'
   in return (gameBoards)

main :: IO ()
main = do
  boards <- day4Boards
  draws <- day4Draws
  print (runGame boards (map prepareMarkedBoard boards) draws)
  print (runGameLoser boards (map prepareMarkedBoard boards) draws)

prepareMarkedBoard :: GameBoard -> MarkedGameBoard
prepareMarkedBoard = map (map (\element -> (element, False)))

convertBackToBoard :: MarkedGameBoard -> GameBoard
convertBackToBoard = map (map (fst))

drawNumber :: Integer -> MarkedGameBoard -> MarkedGameBoard
drawNumber number = map (map (\(element, current) -> (element, current || (number == element))))

checkRowsForWin :: MarkedGameBoard -> Bool
checkRowsForWin = any (all (\(_, isPicked) -> isPicked))

transposeBoard :: MarkedGameBoard -> MarkedGameBoard
transposeBoard scoreboard =
  let boardLength = length (head scoreboard) - 1
   in map (\curIdx -> map (!! curIdx) scoreboard) [0 .. boardLength]

countScoreFromUnMarked :: MarkedGameBoard -> Integer
countScoreFromUnMarked = sum . map fst . filter (not . snd) . concat

runGame :: [GameBoard] -> [MarkedGameBoard] -> [Integer] -> Maybe Integer
runGame _ _ [] = Nothing
runGame boards lastMarkedBoards (curDraw : rest) =
  let currentMarkedBoards = (map (drawNumber curDraw) lastMarkedBoards)
      currentWinners = filter (\markedBoard -> checkRowsForWin markedBoard || checkRowsForWin (transposeBoard markedBoard)) currentMarkedBoards
   in case currentWinners of
        winningBoard : _ -> Just (curDraw * countScoreFromUnMarked winningBoard)
        [] -> runGame boards (map (drawNumber curDraw) currentMarkedBoards) rest

runGameLoser :: [GameBoard] -> [MarkedGameBoard] -> [Integer] -> Maybe Integer
runGameLoser _ _ [] = Nothing
runGameLoser boards lastMarkedBoards (curDraw : rest) =
  let currentMarkedBoards = (map (drawNumber curDraw) lastMarkedBoards)
      currentLosers = filter (\markedBoard -> not (checkRowsForWin markedBoard || checkRowsForWin (transposeBoard markedBoard))) currentMarkedBoards
   in if (length currentLosers) == 1
        then runGame (map convertBackToBoard currentLosers) currentLosers rest
        else runGameLoser boards (map (drawNumber curDraw) currentMarkedBoards) rest

-- DEBUG STUFF

main' :: Maybe Integer
main' = runGame testGame (map prepareMarkedBoard testGame) testDraws

mainLoser' :: Maybe Integer
mainLoser' = runGameLoser testGame (map prepareMarkedBoard testGame) testDraws

debugGame :: [MarkedGameBoard]
debugGame = scanr (\curDraw acc -> (drawNumber curDraw acc)) (prepareMarkedBoard testBoardB) testDraws

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

testDraws :: [Integer]
testDraws = [7, 4, 9, 5, 11, 17, 23, 2, 0, 14, 21, 24, 10, 16, 13, 6, 15, 25, 12, 22, 18, 20, 8, 19, 3, 26, 1]

runGamePerBoard :: MarkedGameBoard -> [Integer] -> Integer
runGamePerBoard markedBoard [] = -1
runGamePerBoard markedBoard (curDraw : rest) =
  let latestMarkedBoard = drawNumber curDraw markedBoard
   in if checkRowsForWin latestMarkedBoard || checkRowsForWin (transposeBoard latestMarkedBoard)
        then curDraw * countScoreFromUnMarked latestMarkedBoard
        else runGamePerBoard (drawNumber curDraw markedBoard) rest