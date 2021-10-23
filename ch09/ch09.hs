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
capFirst (x : xs) = toUpper x : xs

capAll :: [Char] -> [Char]
capAll [] = ""
capAll (x : xs) = toUpper x : capAll xs

getFirstCap :: [Char] -> Char
getFirstCap = head . capFirst

myOr :: [Bool] -> Bool
myOr [] = False
myOr (x : xs) = x || myOr xs

myAny :: (a -> Bool) -> [a] -> Bool
myAny f [] = False
myAny f (x : xs) = f x || myAny f xs

myElem :: Eq a => a -> [a] -> Bool
myElem _ [] = False
myElem a (x : xs) = a == x || myElem a xs

myElem2 :: Eq a => a -> [a] -> Bool
myElem2 a = myAny ((==) a)

myReverse :: [a] -> [a]
myReverse [] = []
myReverse xs =
  go xs []
  where
    go (x : xs') lst = case xs' of
      [] -> x : lst
      _ -> go xs' (x : lst)

mySquish :: [[a]] -> [a]
mySquish [] = []
mySquish xss =
  go xss []
  where
    go (xs : xss') lst = case xss' of
      [] -> xs ++ lst
      _ -> xs ++ (go xss' lst)

mySquishMap :: (a -> [b]) -> [a] -> [b]
mySquishMap f [] = []
mySquishMap f xss =
  go xss []
  where
    go (xs : xss') lst = case xss' of
      [] -> (f xs) ++ lst
      _ -> (f xs) ++ (go xss' lst)

squishAgain :: [[b]] -> [b]
squishAgain = mySquishMap id

minmax :: Ordering -> (a -> a -> Ordering) -> [a] -> a
minmax _ _ [] = undefined
minmax o f (x : xs)
  | null xs = x
  | otherwise =
    go xs x
  where
    go xs' cur = case xs' of
      [] -> cur
      (a : []) -> if (f a cur) == o then a else cur
      (a : as) -> go as (if (f a cur) == o then a else cur)

myMaxBy :: (a -> a -> Ordering) -> [a] -> a
myMaxBy = minmax GT

myMinBy :: (a -> a -> Ordering) -> [a] -> a
myMinBy = minmax LT

myMaximum :: (Ord a) => [a] -> a
myMaximum = myMaxBy compare

myMinimum :: (Ord a) => [a] -> a
myMinimum = myMinBy compare