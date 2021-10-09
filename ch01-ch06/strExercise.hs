module StrExercise where

addExclamation :: [Char] -> [Char]
addExclamation s = s ++ "!"

fifthLetter :: [a] -> a
fifthLetter s = s !! 4

thirdLetter :: [a] -> a
thirdLetter s = s !! 2

letterIndex = (!!) s
  where
    s = "Curry is awesome!"

dropNine = drop 9

rvrs = drop 9 s ++ " " ++ (take 2 (drop 6 s)) ++ " " ++ take 5 s
  where
    s = "Curry is awesome"
