module Person where

import Control.Monad (forever)

type Name = String

type Age = Integer

data Person = Person Name Age deriving (Show)

data PersonInvalid
  = NameEmpty
  | AgeTooLow
  | PersonInvalidUnknown String
  deriving (Show)

mkPerson ::
  Name ->
  Age ->
  Either PersonInvalid Person
mkPerson name age
  | name /= "" && age > 0 = Right (Person name age)
  | name == "" = Left NameEmpty
  | age <= 0 = Left AgeTooLow
  | otherwise = Left (PersonInvalidUnknown ("Name was: " ++ show name ++ " Age was: " ++ show age))

gimmePerson :: IO ()
gimmePerson = forever $ do
  putStrLn "Name? "
  name <- getLine
  putStrLn "Age? "
  age <- getLine
  case (mkPerson name (read age :: Integer)) of
    Left err -> putStrLn (show err)
    Right person -> putStrLn ("Yay: " ++ show person)
