module TopOrLocal where

topLevelFunc :: Integer -> Integer
topLevelFunc x =
  x + woot + topLevelValue
  where
    woot = 10

topLevelValue :: Integer
topLevelValue = 5