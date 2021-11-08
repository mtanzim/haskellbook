module Ch12Exercises where

notThe :: String -> Maybe String
notThe "the" = Nothing
notThe s = Just s

replaceThe :: String -> String
replaceThe = unwords . go . words
  where
    getHead h = case (notThe h) of
      Just s -> s
      Nothing -> "a"
    go ws = case ws of
      (head : tail') -> getHead head : (go tail')
      [] -> []
