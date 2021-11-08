module Ch12Exercises where

vowels :: [Char]
vowels = "aeiouAEIOU"

notThe :: String -> Maybe String
notThe "the" = Nothing
notThe s = Just s

replaceThe :: String -> String
replaceThe = unwords . go . words
  where
    getHead h = case (notThe h) of
      Just s -> s
      Nothing -> "a"
    go ws = case ws of
      (head : tail') -> getHead head : (go tail')
      [] -> []

countTheBeforeVowel :: String -> Integer
countTheBeforeVowel = flip go 0 . words
  where
    go xs count = case xs of
      [] -> count
      (head : []) -> count
      (head : neck : tail) -> if (notThe head) == Nothing && elem (neck !! 0) vowels then go (neck : tail) (count + 1) else go (neck : tail) count

isVowel :: Char -> Bool
isVowel = flip elem vowels

countVowels :: String -> Integer
countVowels = foldr (\c acc -> if isVowel c then acc + 1 else acc) 0

-- TODO: this assumes everything except a vowel is a consonant which is inaccurate
countConsonants :: String -> Integer
countConsonants = foldr (\c acc -> if isVowel c then acc else acc + 1) 0

newtype Word' = Word' String deriving (Eq, Show)

mkWord :: String -> Maybe Word'
mkWord s = if countConsonants s < countVowels s then Nothing else Just (Word' s)

data Nat = Zero | Succ Nat deriving (Eq, Show)

natToInteger :: Nat -> Integer
natToInteger Zero = 0
natToInteger (Succ Zero) = 1
natToInteger (Succ (Succ c)) = 1 + natToInteger (Succ c)

integerToNat :: Integer -> Maybe Nat
integerToNat i
  | i < 0 = Nothing
  | otherwise = Just (go i)
  where
    go i = case i of
      0 -> Zero
      i -> Succ (go (i -1))

isJust :: Maybe a -> Bool
isJust (Just _) = True
isJust _ = False

isNothing :: Maybe a -> Bool
isNothing = not . isJust

mayybee :: b -> (a -> b) -> Maybe a -> b
mayybee b aToB mba = case mba of
  Nothing -> b
  Just a -> aToB a

fromMaybe :: a -> Maybe a -> a
fromMaybe a mba = case mba of
  Nothing -> a
  Just a' -> a'

listToMaybe :: [a] -> Maybe a
listToMaybe [a] = Just a
listToMaybe [] = Nothing
listToMaybe _ = Nothing

maybeToList :: Maybe a -> [a]
maybeToList (Just a) = [a]
maybeToList Nothing = []

catMaybes :: [Maybe a] -> [a]
catMaybes [] = []
catMaybes (head : tail) = case head of
  Just a -> a : catMaybes tail
  Nothing -> catMaybes tail

flipMaybe :: [Maybe a] -> Maybe [a]
flipMaybe [] = Just []
flipMaybe lst = if length (catMaybes lst) == length lst then Just (catMaybes lst) else Nothing