module ReplaceExperiment where

replaceWithP :: b -> Char
replaceWithP = const 'p'

lms :: [Maybe [Char]]
lms = [Just "Ave", Nothing, Just "woohoo"]

replaceWithP' :: [Maybe [Char]] -> Char
replaceWithP' = replaceWithP

liftedReplace :: Functor f => f a -> f Char
liftedReplace = fmap replaceWithP

liftedReplace' :: [Maybe [Char]] -> [Char]
liftedReplace' = liftedReplace

twiceLifted :: (Functor f1, Functor f2) => f2 (f1 a) -> f2 (f1 Char)
twiceLifted = (fmap . fmap) replaceWithP

twiceLifted' ::  [Maybe [Char]] ->  [Maybe Char]
twiceLifted' = twiceLifted

thriceLifted :: (Functor f1, Functor f2, Functor f3) =>f3 (f2 (f1 a)) -> f3 (f2 (f1 Char))
thriceLifted = (fmap . fmap. fmap) replaceWithP

thriceLifted' ::  [Maybe [Char]] ->  [Maybe [Char]]
thriceLifted' = thriceLifted

main :: IO()
main = do
    putStr "lms: "
    print (lms)
    putStr "replaceWithP lms: "
    print (replaceWithP' lms)
    putStr "liftedReplace' lms: "
    print (liftedReplace' lms)
    putStr "twiceLifted' lms: "
    print (twiceLifted' lms)
    putStr "thriceLifted' lms: "
    print (thriceLifted' lms)