module Validation where

import Control.Applicative

validateLength :: Int -> String -> Maybe String
validateLength maxLen s =
  if length s > maxLen
    then Nothing
    else Just s

newtype Name = Name String deriving (Eq, Show)

newtype Address = Address String deriving (Eq, Show)

mkName :: String -> Maybe Name
mkName s = fmap Name (validateLength 3 s)

mkAddress :: String -> Maybe Address
mkAddress a = fmap Address (validateLength 100 a)

data Person = Person Name Address deriving (Eq, Show)

mkPerson :: String -> String -> Maybe Person
mkPerson n a = case mkName n of
  Nothing -> Nothing
  Just n' ->
    case mkAddress a of
      Nothing -> Nothing
      Just a' -> Just (Person n' a')

mkPerson' :: String -> String -> Maybe Person
mkPerson' n a =
  --   (fmap Person (mkName n)) <*> mkAddress a
  Person <$> mkName n <*> mkAddress a

data Cow = Cow
  { name :: String,
    age :: Int,
    weight :: Int
  }

noEmpty :: [Char] -> Maybe [Char]
noEmpty "" = Nothing
noEmpty s = Just s

noNegative :: Int -> Maybe Int
noNegative n
  | n >= 0 = Just n
  | otherwise = Nothing

cowFromString :: String -> Int -> Int -> Maybe Cow
cowFromString name age weight =
  Cow <$> noEmpty name <*> noNegative age <*> noNegative weight

cowFromString' :: String -> Int -> Int -> Maybe Cow
cowFromString' name age weight =
  liftA3 Cow (noEmpty name) (noNegative age) (noNegative weight)

main :: Maybe Person
main =
  let name = mkName "babe"
      addy = mkAddress "old mcdonald"
   in fmap Person name <*> addy