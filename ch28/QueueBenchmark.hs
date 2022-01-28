module QueueBenchmark where

import Criterion.Main
import qualified Data.Sequence as S

data Queue a = Queue
  { enqueue :: [a],
    dequeue :: [a]
  }
  deriving (Eq, Show)

push :: a -> Queue a -> Queue a
push a (Queue curEq curDq) =
  let updatedEq = a : curEq
   in Queue updatedEq curDq

pop :: Queue a -> Maybe (a, Queue a)
pop (Queue [] []) = Nothing
pop (Queue [a] [_]) = Just (a, Queue [] [])
pop (Queue curEq []) =
  let updatedDq = reverse curEq
      val = head updatedDq
      updatedDq' = tail updatedDq
   in Just (val, Queue [] updatedDq')
pop (Queue curEq curDq) = Just (head curDq, Queue curEq (tail curDq))

simplePush :: a -> [a] -> [a]
simplePush a lst = lst ++ [a]

simplePop :: [a] -> Maybe (a, [a])
simplePop [] = Nothing
simplePop lst = Just (head lst, tail lst)

emptyQ :: Queue a
emptyQ = Queue [] []

toQueue :: [a] -> Queue a
toQueue lst = go lst emptyQ
  where
    go [] q = q
    go (x : xs) q = go xs (push x q)

boringDance :: Int -> [Int]
boringDance n =
  let z = zLst
   in go n z 33
  where
    go 0 q _ = q
    go n q lastV =
      if even n
        then
          let val = simplePop q
           in case val of
                Just (v, q') -> go (n - 1) q' v
                Nothing -> q
        else go (n - 1) (simplePush lastV q) lastV

popLockDrop :: Int -> Queue Int
popLockDrop n =
  let z = toQueue zLst
   in go n z 33
  where
    go 0 q _ = q
    go n q lastV =
      if even n
        then
          let val = pop q
           in case val of
                Just (v, q') -> go (n - 1) q' v
                Nothing -> q
        else go (n - 1) (push lastV q) lastV

seqPop :: S.Seq a -> Maybe (a, S.Seq a)
seqPop s = case (S.lookup 0 s) of
  Just v -> Just (v, S.deleteAt 0 s)
  Nothing -> Nothing

seqPush :: a -> S.Seq a -> S.Seq a
seqPush a s = S.insertAt (S.length s) a s

-- TODO: polymorphic function to cater all 3 dance functions?
lineDance :: Int -> S.Seq Int
lineDance n =
  let z = S.fromList zLst
   in go n z 33
  where
    go 0 q _ = q
    go n q lastV =
      if even n
        then
          let val = seqPop q
           in case val of
                Just (v, q') -> go (n - 1) q' v
                Nothing -> q
        else go (n - 1) (seqPush lastV q) lastV

zLst :: [Int]
zLst = [1 .. 100]

main :: IO ()
main =
  defaultMain
    [ bench "two stack Q" $ whnf popLockDrop 123456,
      bench "sequence" $ whnf lineDance 123456,
      bench "one stack Q" $ whnf boringDance 123456
    ]

-- List size = 10
-- benchmarking two stack Q
-- time                 138.2 ms   (127.2 ms .. 149.4 ms)
--                      0.996 R²   (0.990 R² .. 1.000 R²)
-- mean                 141.8 ms   (138.8 ms .. 144.9 ms)
-- std dev              4.532 ms   (3.212 ms .. 6.202 ms)
-- variance introduced by outliers: 12% (moderately inflated)

-- benchmarking one stack Q
-- time                 167.3 ms   (150.2 ms .. 174.2 ms)
--                      0.996 R²   (0.983 R² .. 1.000 R²)
-- mean                 180.7 ms   (174.9 ms .. 194.6 ms)
-- std dev              12.00 ms   (2.261 ms .. 17.29 ms)
-- variance introduced by outliers: 15% (moderately inflated)

-- List size = 100
-- benchmarking two stack Q
-- time                 184.7 ms   (143.9 ms .. 221.8 ms)
--                      0.961 R²   (0.872 R² .. 0.999 R²)
-- mean                 157.4 ms   (145.8 ms .. 171.7 ms)
-- std dev              20.23 ms   (10.59 ms .. 28.82 ms)
-- variance introduced by outliers: 40% (moderately inflated)

-- benchmarking one stack Q
-- time                 742.0 ms   (699.9 ms .. 774.6 ms)
--                      1.000 R²   (0.999 R² .. 1.000 R²)
-- mean                 738.6 ms   (724.2 ms .. 744.7 ms)
-- std dev              10.26 ms   (3.736 ms .. 14.00 ms)
-- variance introduced by outliers: 19% (moderately inflated)

-- List Size = 1000
-- benchmarking two stack Q
-- time                 139.7 ms   (130.6 ms .. 145.9 ms)
--                      0.996 R²   (0.985 R² .. 1.000 R²)
-- mean                 143.6 ms   (140.3 ms .. 148.2 ms)
-- std dev              5.744 ms   (3.195 ms .. 8.919 ms)
-- variance introduced by outliers: 12% (moderately inflated)

-- benchmarking one stack Q
-- time                 4.273 s    (1.855 s .. 5.899 s)
--                      0.966 R²   (0.880 R² .. 1.000 R²)
-- mean                 4.495 s    (4.244 s .. 4.808 s)
-- std dev              323.7 ms   (130.0 ms .. 445.6 ms)
-- variance introduced by outliers: 20% (moderately inflated)

-- with Sequence, List size = 100
-- benchmarking two stack Q
-- time                 138.9 ms   (133.8 ms .. 146.0 ms)
--                      0.997 R²   (0.989 R² .. 1.000 R²)
-- mean                 137.8 ms   (135.6 ms .. 140.5 ms)
-- std dev              3.939 ms   (3.017 ms .. 5.383 ms)
-- variance introduced by outliers: 11% (moderately inflated)

-- benchmarking sequence
-- time                 129.2 ms   (128.0 ms .. 130.7 ms)
--                      1.000 R²   (1.000 R² .. 1.000 R²)
-- mean                 130.3 ms   (129.3 ms .. 132.8 ms)
-- std dev              2.320 ms   (574.1 μs .. 3.545 ms)
-- variance introduced by outliers: 11% (moderately inflated)

-- benchmarking one stack Q
-- time                 670.3 ms   (514.9 ms .. 808.9 ms)
--                      0.994 R²   (0.976 R² .. 1.000 R²)
-- mean                 704.2 ms   (676.8 ms .. 727.7 ms)
-- std dev              28.68 ms   (23.20 ms .. 32.74 ms)
-- variance introduced by outliers: 19% (moderately inflated)
