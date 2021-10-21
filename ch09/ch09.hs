module Ch09 where

import Text.Parsec (endBy)
import PoemLines (myChunks)

safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (a : _) = Just a

-- TODO: better way to do this?
eftBool :: Bool -> Bool -> [Bool]
eftBool start end =
  case (start, end) of
    (True, False) -> []
    (False, True) -> [False, True]
    (False, False) -> [False]
    (True, True) -> [True]

-- TODO: idiomatic to reverse at the end?
-- TODO: How to share this exact code between types?
eftOrdering :: Ordering -> Ordering -> [Ordering]
eftOrdering start end = go start end []
  where
    go start' end' lst
      | start' > end' = reverse lst
      | otherwise = go (succ start') end' (start' : lst)

eftInt :: Int -> Int -> [Int]
eftInt start end = go start end []
  where
    go start' end' lst
      | start' > end' = reverse lst
      | otherwise = go (succ start') end' (start' : lst)

eftChar :: Char -> Char -> [Char]
eftChar start end = go start end []
  where
    go start' end' lst
      | start' > end' = reverse lst
      | otherwise = go (succ start') end' (start' : lst)

-- thy fearful symmetry
mywords :: String -> [String]
mywords = myChunks ' '