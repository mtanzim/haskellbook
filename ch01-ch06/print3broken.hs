module Print3Broken where

printSecond myGreeting = do
    putStrLn myGreeting


main = do
    putStrLn greeting
    printSecond argGreeting
    where 
        greeting = "Yarrr"
        argGreeting = "pirate"
