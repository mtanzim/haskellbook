module ForceBottom where

x = undefined

y :: [Char]
y = "blah"

main :: IO ()
main = do
  print $ x `seq` (snd (x, y))