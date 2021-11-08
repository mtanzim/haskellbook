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