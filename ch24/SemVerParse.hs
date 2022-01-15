{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module SemVerParse where

import Control.Applicative
import Text.Trifecta

data NumberOrString = NOSS String | NOSI Integer deriving (Eq, Show)

type Major = Integer

type Minor = Integer

type Patch = Integer

type Release = [NumberOrString]

type Metadata = [NumberOrString]

data SemVer = SemVer Major Minor Patch Release Metadata deriving (Eq, Show)

parseVersions :: Parser (Major, Minor, Patch)
parseVersions = do
  major <- integer
  _ <- char '.'
  minor <- integer
  _ <- char '.'
  patch <- integer
  return (major, minor, patch)

parseRest :: Parser NumberOrString
parseRest = do
  v <- (NOSI <$> decimal) <|> (NOSS <$> some letter)
  skipMany (oneOf ".")
  return v

parseSemVer :: Parser SemVer
parseSemVer = do
  (major, minor, patch) <- parseVersions
  _ <- optional (char '-')
  release <- option [] (some parseRest)
  _ <- optional (char '+')
  meta <- option [] (some parseRest)
  return $ SemVer major minor patch release meta

ps = parseString

psv :: String -> Result SemVer
psv = ps parseSemVer mempty

-- TODO: ordering
-- TODO: rest of this painful chapter :_(

main :: IO ()
main = do
  print $ psv "2.1.1"
  print $ psv "1.0.0-x.7.z.92"
  print $ psv "1.0.0-gamma+002"
  print $ psv "1.0.0-beta+oof.sha.41af286"
