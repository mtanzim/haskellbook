module Ch09 where

import Data.Char

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

-- comprehend thy list

mySqr :: [Integer]
mySqr = [x ^ 2 | x <- [1 .. 5]]

myCube = [x ^ 3 | x <- [1 .. 5]]

mySquareCubes :: Int -> [(Integer, Integer)]
mySquareCubes = flip take [(x, y) | x <- mySqr, y <- myCube, x < 50, y < 50]

-- filtering
multOf3 :: [Integer] -> [Integer]
multOf3 = filter (\x -> rem x 3 == 0)

multOf3Len :: [Integer] -> Int
multOf3Len = length . (filter (\x -> rem x 3 == 0))

myFilter :: String -> [String]
myFilter = filterFn . words
  where
    filterFn = filter f where
    f w
      | w == "the" = False
      | w == "a" = False
      | w == "an" = False
      | otherwise = True

-- zipping
-- TODO: debug the no-exhaustive pattern match warnings
myZip :: [a] -> [b] -> [(a, b)]
myZip as bs
  | null as = []
  | null bs = []
  | otherwise =
    reverse (go as bs [])
  where
    go (a : as') (b : bs') curLst =
      case (as', bs') of
        ([], _) -> ((a, b) : curLst)
        (_, []) -> ((a, b) : curLst)
        _ -> go as' bs' ((a, b) : curLst)

myZipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
myZipWith f as bs
  | null as = []
  | null bs = []
  | otherwise =
    reverse (go as bs [])
  where
    go (a : as') (b : bs') curLst =
      case (as', bs') of
        ([], _) -> (f a b : curLst)
        (_, []) -> (f a b : curLst)
        _ -> go as' bs' (f a b : curLst)

myZip2 :: [a] -> [b] -> [(a, b)]
myZip2 = myZipWith f
  where
    f a b = (a, b)

-- chapter exercises
capFirst :: [Char] -> [Char]
capFirst [] = ""
capFirst (x:xs) = toUpper x : xs

capAll :: [Char] -> [Char]
capAll [] = ""
capAll (x:xs) = toUpper x : capAll xs

getFirstCap :: [Char] -> Char
getFirstCap = head . capFirst