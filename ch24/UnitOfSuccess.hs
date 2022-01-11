module UnitOfSuccess where

import Text.Trifecta

parser :: Parser ()
parser = integer >> eof

-- https://stackoverflow.com/a/2486591/9641551
parser' :: Parser Integer
parser' = do
  value <- integer
  eof
  return value

main = do
  print $ parseString integer mempty "123abc"
  print $ parseString parser mempty "123abc"
  print $ parseString parser mempty "123"
  print $ parseString parser' mempty "123"
  print $ parseString parser' mempty "123abc"
