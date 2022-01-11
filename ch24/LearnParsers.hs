module LearnParsers where

import Control.Applicative
import Text.Trifecta

stop :: Parser a
stop = unexpected "stop"

one :: Parser Char
one = char '1'

oneDone :: Parser ()
oneDone = one >> eof

one' :: Parser b
one' = one >> stop

oneTwo :: Parser Char
oneTwo = char '1' >> char '2'

oneTwo' :: Parser b
oneTwo' = oneTwo >> stop

oneTwoThreeDone :: Parser ()
oneTwoThreeDone = oneTwo >> char '3' >> eof

parse123String :: Parser String
parse123String = string "123" <|> string "12" <|> string "1"

-- TODO: this doesn't work :(
parse123Char :: Parser Char
parse123Char = (char '1' >> char '2' >> char '3') <|> (char '1' >> char '2') <|> char '1'

testParse :: Parser Char -> IO ()
testParse p = print $ parseString p mempty "123"

pNL :: [Char] -> IO ()
pNL s = putStrLn ('\n' : s)

main :: IO ()
main = do
  pNL "stop:"
  testParse stop
  pNL "one:"
  testParse one
  pNL "one':"
  testParse one'
  pNL "oneTwo:"
  testParse oneTwo
  pNL "oneTwo':"
  testParse oneTwo'
  pNL "oneDone"
  print $ parseString oneDone mempty "123"
  pNL "oneDone success"
  print $ parseString oneDone mempty "1"
  pNL "oneTwoThreeDone success"
  print $ parseString oneTwoThreeDone mempty "123"
  pNL "oneTwoThreeDone failure"
  print $ parseString oneTwoThreeDone mempty "1234"
  pNL "oneTwoThreeDone failure 2"
  print $ parseString oneTwoThreeDone mempty "12"
  pNL "parse123String"
  print $ parseString parse123String mempty "123"
