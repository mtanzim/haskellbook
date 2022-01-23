module Morra where

import Data.Bool (Bool)
import Data.Maybe (fromMaybe)
import System.Random
import Text.Read (readMaybe)
import Control.Monad (forever)

data PlayerState = Odd | Even deriving (Eq, Show)

data Config = Config
  { player1 :: PlayerState
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
  case (player1 config, sum `mod` 2) of
    (Even, 0) -> True
    (Odd, 1) -> True
    _ -> False

main :: IO ()
main = do
  putStrLn "Pick odd or even: 0 for even, 1 for Odd"
  ans <- getLine
  let config = getConfig ans
  case config of
    Nothing -> error "Invalid input"
    Just con -> forever $ do
      print config
      putStrLn "How many fingers? (0 to 5)"
      fingers <- getLine
      cpuVal <- randomRIO (0 :: Int, 5 :: Int)
      let fingersN = getFingers fingers
      case fingersN of
        Nothing -> putStrLn "Invalid input, try again"
        Just n -> do
          let sum = n + cpuVal
          putStrLn $ "CPU picked: " ++ show cpuVal
          putStrLn $ "Sum: " ++ show sum
          putStr "Did you win? "
          print $ didPlayerWin con sum
          putStrLn "\n"
