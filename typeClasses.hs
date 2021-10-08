module TypeClasses where

import Data.List (sort)

data Person = Person Bool
  deriving (Show)

printPerson :: Person -> IO ()
printPerson person = putStrLn (show person)

instance Eq Mood2 where
  (==) Woot2 Woot2 = True
  (==) Blah2 Blah2 = True
  (==) _ _ = False

data Mood2 = Blah2 | Woot2
  deriving (Ord, Show)

settleDown :: Mood2 -> Mood2
settleDown x =
  if x == Woot2
    then Blah2
    else x

type Subject = String

type Verb = String

type Object = String

data Sentence
  = Sentence Subject Verb Object
  deriving (Eq, Show)

s1 = Sentence "dogs" "drool"

s2 = Sentence "Julie" "loves" "dogs"

data Rocks
  = Rocks String
  deriving (Eq, Show)

data Yeah
  = Yeah Bool
  deriving (Eq, Show)

data Papu
  = Papu Rocks Yeah
  deriving (Eq, Show)

instance Ord Papu where
  compare (Papu (Rocks s) (Yeah b)) (Papu (Rocks s') (Yeah b')) = if b then GT else compare s' s

phew = Papu (Rocks "chases") (Yeah False)

truth = Papu (Rocks "chomskydoz") (Yeah True)

equalityForall :: Papu -> Papu -> Bool
equalityForall p p' = p == p'

comparePapus :: Papu -> Papu -> Bool
comparePapus p p' = p > p'

f :: RealFrac a => a
f = 1.0

freud :: Int -> Int
freud x = x

myX :: Int
myX = 1 :: Int

-- sigmund :: Num a => a -> a
-- sigmund x = myX

jung :: [Int] -> Int
jung xs = head (sort xs)

young :: Ord a => [a] -> a
young xs = head (sort xs)

-- mySort :: [Char] -> [Char]
mySort :: Ord a => [a] -> [a]
mySort = sort

signifier :: Ord a => [a] -> a
signifier xs = head (mySort xs)
