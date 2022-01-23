module Morra where

import Data.Maybe (fromMaybe)
import Text.Read (readMaybe)

-- data Odds = Odd

-- data Evens = Even

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

main :: IO ()
main = do
  putStrLn "Pick odd or even: 0 for even, 1 for Odd"
  ans <- getLine
  let config = getConfig ans
  case config of
    Nothing -> error "Invalid input"
    Just con -> print config
  putStrLn "How many fingers? (0 to 5)"
  fingers <- getLine
  let fingersN = getFingers fingers
  case fingersN of 
    Nothing -> error "Invalid input"
    Just n -> putStrLn fingers
