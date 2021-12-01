module AdventDay1 where

testInput :: [Integer]
testInput = [199, 200, 208, 210, 200, 207, 240, 269, 260, 263]

day1Input :: IO [Integer]
day1Input = do
  dict <- readFile "day1Input.txt"
  return (map (\x -> read x :: Integer) (lines dict))

numIncreases :: (Ord a, Num p) => [a] -> p
numIncreases lst = go lst 0
  where
    go lst' count =
      case lst' of
        head : neck : tail -> if neck > head then go (neck : tail) (count + 1) else go (neck : tail) count
        _ -> count

sums :: Num a => [a] -> [a]
sums lst = reverse (go lst [])
  where
    go lst' sumLst =
      case lst' of
        head : neck : torso : tail -> go (neck : torso : tail) ((head + neck + torso) : sumLst)
        _ -> sumLst

numIncreasesWindow :: [Integer] -> Integer
numIncreasesWindow = numIncreases . sums

main :: IO ()
main = do
  input <- day1Input
  print (numIncreases input)
  print (numIncreasesWindow input)
