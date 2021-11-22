module Main where

import Control.Monad (forever)
import Data.Char (toLower)
import Data.List (intersperse)
import Data.Maybe (isJust)
import System.Exit (exitSuccess)
import System.IO (BufferMode (NoBuffering), hSetBuffering, stdout)
import System.Random (randomRIO)
import Test.Hspec
import Test.QuickCheck

type WordList = [String]

allWords :: IO WordList
allWords = do
  dict <- readFile "data/dict.txt"
  return (lines dict)

minWordLength :: Int
minWordLength = 5

maxWordLength :: Int
maxWordLength = 9

numGuesses :: Int
numGuesses = maxWordLength * 2

gameWords :: IO WordList
gameWords = do
  aw <- allWords
  return (filter gameLength aw)
  where
    gameLength w =
      let l = length (w :: String)
       in l >= minWordLength && l < maxWordLength

randomWord :: WordList -> IO String
randomWord wl = do
  randomIndex <- randomRIO (0, length wl - 1)
  return (wl !! randomIndex)

randomWord' :: IO String
randomWord' = gameWords >>= randomWord

data Puzzle = Puzzle String [Maybe Char] [Char] Int deriving (Eq)

instance Show Puzzle where
  show (Puzzle _ discovered guessed guessesLeft) =
    (intersperse ' ' (fmap renderPuzzleChar discovered))
      ++ "\nGuessed so far: "
      ++ guessed
      ++ "\nNumber of guesses left: "
      ++ show guessesLeft
      ++ "\n\n"

freshPuzzle :: String -> Puzzle
freshPuzzle s = Puzzle s (fmap (const Nothing) s) [] numGuesses

charInWord :: Puzzle -> Char -> Bool
charInWord (Puzzle s _ _ _) c = c `elem` s

alreadyGuessed :: Puzzle -> Char -> Bool
alreadyGuessed (Puzzle _ _ guessed _) c = c `elem` guessed

renderPuzzleChar :: Maybe Char -> Char
renderPuzzleChar Nothing = '_'
renderPuzzleChar (Just c) = c

fillInCharacter :: Puzzle -> Char -> Bool -> Puzzle
fillInCharacter (Puzzle word filledInSoFar s guessesLeft) c isCorrect = Puzzle word newFilledInSofar (c : s) newGuessesLeft
  where
    newFilledInSofar = zipWith (zipper c) word filledInSoFar
    zipper guessed wordChar guessChar = if wordChar == guessed then Just wordChar else guessChar
    newGuessesLeft = if isCorrect then guessesLeft else (guessesLeft - 1)

handleGuessPure :: Puzzle -> Char -> (String, Puzzle)
handleGuessPure puzzle guess =
  case (charInWord puzzle guess, alreadyGuessed puzzle guess) of
    (_, True) -> ("You already guessed that, pick something else", puzzle)
    (True, _) ->
      ("Word found, filling it in!", fillInCharacter puzzle guess True)
    (False, _) -> ("Try again", fillInCharacter puzzle guess False)

handleGuess :: Puzzle -> Char -> IO Puzzle
handleGuess puzzle guess = do
  putStrLn $ "Your guess was: " ++ [guess]
  putStrLn msg
  return newPuzzle
  where
    (msg, newPuzzle) = handleGuessPure puzzle guess

gameOver :: Puzzle -> IO ()
gameOver (Puzzle wordToGuess _ _ guessesLeft) =
  if guessesLeft == 0
    then do
      putStrLn "You lose!"
      putStrLn $ "The word was: " ++ wordToGuess
      exitSuccess
    else return ()

gameWin :: Puzzle -> IO ()
gameWin (Puzzle _ filledInSoFar _ _) =
  if all isJust filledInSoFar
    then do
      putStrLn "You win!"
      exitSuccess
    else return ()

runGame :: Puzzle -> IO ()
runGame puzzle = forever $ do
  gameOver puzzle
  gameWin puzzle
  putStrLn $ "\n\nCurrent puzzle is: " ++ show puzzle
  putStr "Guess a letter: "
  guess <- getLine
  case guess of
    [c] -> handleGuess puzzle c >>= runGame
    _ -> putStrLn "Guess must be a single character"

test :: IO ()
test = hspec $ do
  describe "fillInCharacter" $ do
    it "guessed correctly" $ do
      fillInCharacter (Puzzle "cat" [Nothing, Nothing, Nothing] [] 20) 'a' True `shouldBe` Puzzle "cat" [Nothing, Just 'a', Nothing] ['a'] 20
    it "guessed incorrectly" $ do
      fillInCharacter (Puzzle "cat" [Nothing, Nothing, Nothing] [] 20) 'z' False `shouldBe` Puzzle "cat" [Nothing, Nothing, Nothing] ['z'] 19
  describe "handleGuessPure" $ do
    it "guessed correctly" $ do
      handleGuessPure puzzle1 char1 `shouldBe` expectedPuzzle1
    it "guessed incorrectly" $ do
      handleGuessPure puzzle2 char2 `shouldBe` expectedPuzzle2
    it "already guessed" $ do
      handleGuessPure puzzle3 char3 `shouldBe` expectedPuzzle3
  where
    puzzle1 = Puzzle "cat" [Nothing, Nothing, Nothing] [] 20
    char1 = 'a'
    expectedPuzzle1 = ("Word found, filling it in!", Puzzle "cat" [Nothing, Just 'a', Nothing] ['a'] 20)

    puzzle2 = puzzle1
    char2 = 'b'
    expectedPuzzle2 = ("Try again", Puzzle "cat" [Nothing, Nothing, Nothing] ['b'] 19)

    puzzle3 = Puzzle "cat" [Nothing, Nothing, Nothing] ['a'] 15
    char3 = 'a'
    expectedPuzzle3 = ("You already guessed that, pick something else", Puzzle "cat" [Nothing, Nothing, Nothing] ['a'] 15)

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  word <- randomWord'
  let puzzle =
        freshPuzzle (fmap toLower word)
  runGame puzzle