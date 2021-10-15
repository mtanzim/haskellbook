module Ch08 where

-- recursion
applyTimes :: (Eq a, Num a) => a -> (b -> b) -> b -> b
applyTimes 0 f b = b
applyTimes n f b = f (applyTimes (n - 1) f b)

-- exercises
sumFrom :: (Eq t, Num t) => t -> t
sumFrom n
  | n == 1 = n
  | otherwise = n + sumFrom (n - 1)

mult :: Integral a => a -> a -> a
mult a b
  | b == 1 = a
  | otherwise = a + mult a (b - 1)

data DividedResult  = 
    Result Integer | DividedByZero
    deriving Show

dividedBy :: Integral a => a -> a -> DividedResult
dividedBy num denom = 
    case denom of
        0 -> DividedByZero
        _ -> Result (go (abs num) (abs denom) 0)
            where
                go n d count
                    | n < d = count * modifier
                    | otherwise = go (n - d) d (count + 1)
                        where modifier =
                                case (num < 0, denom < 0) of
                                (True, True) -> 1
                                (False, False) -> 1
                                _ -> (-1)


mc91 :: (Ord t, Num t) => t -> t
mc91 n
    | n > 100 = n - 10
    | otherwise = mc91(mc91(n+11))

