module Day6 where

testInput :: [Integer]
testInput = [3, 4, 3, 1, 2]

elapse :: [Integer] -> [Integer]
elapse currentFishes =
  let curZeroes = countZeroes currentFishes
      fishesToAdd = map (const 8) [1 .. curZeroes]
   in map (\x -> if x == 0 then 6 else x -1) currentFishes ++ fishesToAdd

countZeroes :: [Integer] -> Integer
countZeroes = foldr (\x acc -> if x == 0 then acc + 1 else acc) 0

fishSimulator :: [Integer] -> Integer -> Int
fishSimulator initState days = length (foldr (\day acc -> elapse acc) initState [1 .. days])

testMain :: Int
testMain = fishSimulator testInput 80