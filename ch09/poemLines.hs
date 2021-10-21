module PoemLines where

firstSen = "Tyger Tyger, burning bright\n"

secondSen = "In the forests of the night\n"

thirdSen = "What immortal hand or eye\n"

fourthSen =
  "Could frame thy fearful\
  \ symmetry?"

sentences :: [Char]
sentences = firstSen ++ secondSen ++ thirdSen ++ fourthSen

shouldEqual =
  [ "Tyger Tyger, burning bright",
    "In the forests of the night",
    "What immortal hand or eye",
    "Could frame thy fearful symmetry?"
  ]

myChunks :: Char -> String -> [String]
myChunks delim s = go s []
  where
    go s' lst
      | null s' = reverse lst
      | otherwise = go (takeTail s') (takeWord s' : lst)
      where
        takeWord = takeWhile (/= delim)
        takeTail = dropWhile (== delim) . dropWhile (/= delim)

myLines :: String -> [String]
myLines = myChunks '\n'

mywords :: String -> [String]
mywords = myChunks ' '

main :: IO ()
main =
  print $
    "Are they equal? "
      ++ show
        ( myLines sentences
            == shouldEqual
        )