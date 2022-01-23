module Morra where

import Control.Monad (Monad (return), forever)
import Control.Monad.State
import Data.Bool (Bool)
import Data.Int (Int)
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import System.Random
import Text.Read (readMaybe)

data PlayerState = Odd | Even deriving (Eq, Show)

data Config = Config
  { playerState :: PlayerState
  }
  deriving (Eq, Show)

getConfig :: String -> Maybe Config
getConfig s = case s of
  "0" -> Just $ Config Even
  "1" -> Just $ Config Odd
  _ -> Nothing

getFingers :: String -> Maybe Int
getFingers s =
  let si = readMaybe s
   in case si of
        Nothing -> Nothing
        Just n -> if n >= 0 && n <= 5 then Just n else Nothing

didPlayerWin :: Config -> Int -> Bool
didPlayerWin config sum =
  case (playerState config, sum `mod` 2) of
    (Even, 0) -> True
    (Odd, 1) -> True
    _ -> False

updateScore :: Bool -> StateT Int IO Int
updateScore won = do
  cv <- get
  if won
    then put $ cv + 1
    else put cv
  get

runGame :: Config -> StateT Int IO ()
runGame con = do
  liftIO $ print con
  liftIO $ putStrLn "How many fingers? (0 to 5)"
  fingers <- liftIO getLine
  cpuVal <- randomRIO (0 :: Int, 5 :: Int)
  let fingersN = getFingers fingers
  case fingersN of
    Nothing -> liftIO $ putStrLn "Invalid input, try again.\n\n"
    Just n -> do
      let sum = n + cpuVal
          playerWon = didPlayerWin con sum
      liftIO $ putStrLn $ "CPU picked: " ++ show cpuVal
      liftIO $ putStrLn $ "Sum: " ++ show sum
      liftIO $ putStrLn $ "Did you win? " ++ show playerWon
      latestScore <- updateScore playerWon
      liftIO $ putStr "Player score: "
      liftIO $ print latestScore
      liftIO $ putStrLn ""

-- https://wiki.haskell.org/Simple_StateT_use
main :: IO ()
main = do
  putStrLn "Pick odd or even: 0 for even, 1 for Odd"
  ans <- getLine
  let config = getConfig ans
  case config of
    Nothing -> error "Invalid input"
    Just con -> do
      runStateT (forever $ runGame con) 0
      return ()
