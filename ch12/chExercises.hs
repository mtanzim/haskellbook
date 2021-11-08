module Ch12Exercises where

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
      (head : neck : tail) -> if (notThe head) == Nothing && elem (neck !! 0) "aeiouAEIOU" then go (neck : tail) (count + 1) else go (neck : tail) count
