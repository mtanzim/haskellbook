module Cow where

data Cow = Cow
  { name :: String,
    age :: Int,
    weight :: Int
  }
  deriving (Eq, Show)

noEmpty :: [Char] -> Maybe [Char]
noEmpty "" = Nothing
noEmpty s = Just s

noNegative :: Int -> Maybe Int
noNegative n
  | n >= 0 = Just n
  | otherwise = Nothing

weightCheck :: Cow -> Maybe Cow
weightCheck c =
  let w = weight c
      n = name c
   in if n == "Bess" && w > 499
        then Nothing
        else Just c

makeSphericalCow :: String -> Int -> Int -> Maybe Cow
makeSphericalCow name' age' weight' =
  case noEmpty name' of
    Nothing -> Nothing
    Just nammy -> case noNegative age' of
      Nothing -> Nothing
      Just agey ->
        case noNegative weight' of
          Nothing -> Nothing
          Just weighty ->
            weightCheck (Cow nammy agey weighty)

-- sugar with do
-- note that this cannot be done with applicative b/c weightCheck expects a Cow and NOT a Maybe Cow
-- See for comparison: https://github.com/mtanzim/haskellbook/blob/53b33690d0b91db77448d0aec92ec6e6b828f88c/ch17/Validation.hs#L52
makeSphericalCow' :: String -> Int -> Int -> Maybe Cow
makeSphericalCow' name' age' weight' = do
  nammy <- noEmpty name'
  agey <- noNegative age'
  weighty <- noNegative weight'
  weightCheck (Cow nammy agey weighty)