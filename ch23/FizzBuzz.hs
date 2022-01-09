module FizzBuzz where

import Control.Monad
import Control.Monad.Trans.State
import qualified Data.DList as DL

fizzBuzz :: Integer -> String
fizzBuzz n
  | n `mod` 15 == 0 = "FizzBuzz"
  | n `mod` 5 == 0 = "Buzz"
  | n `mod` 3 == 0 = "Fizz"
  | otherwise = show n

fizzbuzzList :: [Integer] -> [String]
fizzbuzzList list = execState (mapM_ addResult list) []

fizzbuzzList' :: [Integer] -> DL.DList String
fizzbuzzList' list =
  execState (mapM_ addResult' list) DL.empty

addResult' :: Integer -> State (DL.DList String) ()
addResult' n = do
  xs <- get
  let result = fizzBuzz n
  put (DL.snoc xs result)

addResult :: Integer -> State [String] ()
addResult n = do
  xs <- get
  let result = fizzBuzz n
  put (result : xs)

fizzBuzFromTo :: Integer -> Integer -> [String]
fizzBuzFromTo from to = fizzbuzzList $ enumFromThenTo to (to - 1) from

main :: IO ()
main = do
  --   mapM_ (putStrLn . fizzBuzz) [1 .. 100]
  --   mapM_ putStrLn $ reverse $ fizzbuzzList' [1 .. 100]
--   mapM_ putStrLn $ fizzbuzzList' [1 .. 100]
  mapM_ putStrLn $ fizzBuzFromTo 1 100