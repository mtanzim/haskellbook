module Print3 where

myGreeting :: [Char]
myGreeting = "hello" ++ " world!"

hello :: [Char]
hello = "hello"

world :: [Char]
world = "world!"

main :: IO ()
main = do
  putStrLn myGreeting
  putStrLn secondGreeting
  where
    secondGreeting = concat [hello, " ", world]
