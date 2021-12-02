module AdventDay2 where

type Input = [(String, Integer)]

day2Input :: IO Input
day2Input = do
  directions <- readFile "day2Input.txt"
  return (map (fn . words) (lines directions))
  where
    fn [dir, val] = (dir, read val :: Integer)
    fn _ = undefined

testInput :: Input
testInput = [("forward", 5), ("down", 5), ("forward", 8), ("up", 3), ("down", 8), ("forward", 2)]

calcPos :: Input -> Integer
calcPos lst =
  go lst 0 0
  where
    go lst totalX totalY =
      case lst of
        [] -> totalX * totalY
        (dir, val) : tail -> case dir of
          "forward" -> go tail (totalX + val) totalY
          "up" -> go tail totalX (totalY - val)
          "down" -> go tail totalX (totalY + val)
          _ -> undefined

calcPosWithAim :: Input -> Integer
calcPosWithAim lst =
  go lst 0 0 0
  where
    go lst totalX totalY totalAim =
      case lst of
        [] -> totalX * totalY
        (dir, val) : tail -> case dir of
          "forward" -> go tail (totalX + val) (totalY + totalAim * val) totalAim
          "up" -> go tail totalX totalY (totalAim - val)
          "down" -> go tail totalX totalY (totalAim + val)
          _ -> undefined

main :: IO ()
main = do
  input <- day2Input
  print (calcPos input)
  print (calcPosWithAim input)
