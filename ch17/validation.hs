module Validation where

validateLength :: Int -> String -> Maybe String
validateLength maxLen s =
  if length s > maxLen
    then Nothing
    else Just s

newtype Name = Name String deriving (Eq, Show)

newtype Address = Address String deriving (Eq, Show)

mkName :: String -> Maybe Name
mkName s = fmap Name (validateLength 15 s)

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

main :: Maybe Person
main =
  let name = mkName "babe"
      addy = mkAddress "old mcdonald"
   in fmap Person name <*> addy