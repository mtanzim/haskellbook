{-# LANGUAGE OverloadedStrings #-}

module Fractions where

import Control.Applicative
import Data.Ratio ((%))
import Text.Trifecta

badFraction = "1/0"

alsoBad = "10"

shouldWork = "1/2"

shouldAlsoWork = "2/1"

parseFraction :: Parser Rational
parseFraction = do
  numerator <- decimal
  char '/'
  denominator <- decimal
  return (numerator % denominator)

virtuousFraction :: Parser Rational
virtuousFraction = do
  numerator <- decimal
  char '/'
  denominator <- decimal
  case denominator of
    0 -> fail "Denominator cannot be zero"
    _ -> return (numerator % denominator)

type IntegerOrFraction = Either Rational Integer

type DecimalOrFraction = Either Rational String

parseInt :: Parser Integer
parseInt = do
  v <- integer
  eof
  return v

parseDecimalNum :: Parser String
parseDecimalNum = do
  v1 <- integer
  dot <- char '.'
  v2 <- integer
  eof
  return (show v1 <> [dot] <> show v2)

parseEither :: Parser IntegerOrFraction
parseEither = try (Left <$> virtuousFraction) <|> (Right <$> parseInt)

parseEitherDecimal :: Parser DecimalOrFraction
parseEitherDecimal = try (Left <$> virtuousFraction) <|> (Right <$> parseDecimalNum)

main :: IO ()
main = do
  let parseFraction' = parseString virtuousFraction mempty
      parseFractionInteger = parseString parseEither mempty
      parseFractionDecimal = parseString parseEitherDecimal mempty
  print $ parseFraction' shouldWork
  print $ parseFraction' shouldAlsoWork
  print $ parseFraction' alsoBad
  print $ parseFraction' badFraction
  print $ parseFraction' badFraction
  print $ parseFractionInteger shouldWork
  print $ parseFractionInteger alsoBad
  print $ parseFractionInteger badFraction
  print $ parseFractionDecimal "1/2"
  print $ parseFractionDecimal "1.0"
  print $ parseFractionDecimal "1.05555"
  print $ parseFractionDecimal "1.05555/2323"
  print $ parseFractionDecimal "1/0"
