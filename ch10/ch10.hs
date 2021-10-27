module Ch10 where

fibs :: [Integer]
fibs = 1 : scanl (+) 1 fibs

fibsN :: Int -> Integer
fibsN x = fibs !! x

fibsCapped :: Int -> [Integer]
fibsCapped = (flip take) fibs

fibsFiltered :: [Integer]
fibsFiltered = takeWhile (< 100) fibs

factorial :: [Integer]
factorial = scanl (*) 1 [2 ..]

-- chapter exercise

stops :: [Char]
stops = "pbtdkg"

vowels :: [Char]
vowels = "aeiou"

combineStopsVowels :: [(Char, Char, Char)]
combineStopsVowels = [(s, v, s') | s <- stops, v <- vowels, s' <- stops]

combineStopsVowelsWithFilter :: [(Char, Char, Char)]
combineStopsVowelsWithFilter = [(s, v, s') | s <- stops, s == 'p', v <- vowels, s' <- stops]

nouns = ["Sarah", "Tanzim", "John", "Allan"]

verbs = ["treats", "hates", "misses", "disses"]

combineNounsVerbs :: [([Char], [Char], [Char])]
combineNounsVerbs = [(s, v, s') | s <- nouns, v <- verbs, s' <- nouns]

seekritFunc :: String -> Double
seekritFunc x = totalChars / totalWords
  where
    totalChars = fromIntegral (sum (map length (words x)))
    totalWords = fromIntegral (length (words x))

myAnd :: [Bool] -> Bool
myAnd = foldr f z
  where
    f a b =
      a && b
    z = True

myOr :: [Bool] -> Bool
myOr = foldr f z
  where
    f a b = a || b
    z = False

myAny :: (a -> Bool) -> [a] -> Bool
myAny f = foldr (\a b -> f a || b) False

myElem :: Eq a => a -> [a] -> Bool
myElem x = foldr (\a b -> a == x) False