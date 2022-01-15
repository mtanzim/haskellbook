{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Marshalling where

import Control.Applicative
import Data.Aeson
import Data.ByteString.Lazy (ByteString)
import Data.Scientific (floatingOrInteger)
import Data.Text (Text)
import Text.RawString.QQ

sectionJson :: ByteString
sectionJson =
  [r|
{ "section": {"host": "wikipedia.org"},
"whatisit": {"yellow": "shoes"} }
|]

sectionJson' :: ByteString
sectionJson' =
  [r|
{ "section": {"host": "goolge.ca"},
"whatisit": {"blue": "umbrella"} }
|]

data TestData = TestData
  { section :: Host,
    what :: Color
  }
  deriving (Eq, Show)

newtype Host = Host String deriving (Eq, Show)

type Annotation = String

data Color = Red Annotation | Blue Annotation | Yellow Annotation deriving (Eq, Show)

instance FromJSON TestData where
  parseJSON (Object v) = TestData <$> v .: "section" <*> v .: "whatisit"
  parseJSON _ = fail "Expected an object for test data"

instance FromJSON Host where
  parseJSON (Object v) = Host <$> v .: "host"
  parseJSON _ = fail "Expected object for host"

instance FromJSON Color where
  parseJSON (Object v) =
    (Red <$> v .: "red")
      <|> (Blue <$> v .: "blue")
      <|> (Yellow <$> v .: "yellow")
  parseJSON _ = fail "Expected object for color"

data NumberOrString = Numba Integer | Stringy Text deriving (Eq, Show)

instance FromJSON NumberOrString where
  parseJSON (Number i) =
    case floatingOrInteger i of
      (Left _) -> fail "must be an integral number"
      (Right v) -> return $ Numba v
  parseJSON (String s) = return $ Stringy s
  parseJSON _ = fail "must be number or"

dec :: ByteString -> Maybe NumberOrString
dec = decode

eitherDec :: ByteString -> Either String NumberOrString
eitherDec = eitherDecode

main :: IO ()
main = do
  let d :: Maybe TestData
      d' :: Maybe TestData
      d = decode sectionJson
      d' = decode sectionJson'
  print d
  print d'
  print $ eitherDec "blah"
  print $ eitherDec "123"
  print $ eitherDec "123.34343"
  print $ eitherDec "\"blah\""