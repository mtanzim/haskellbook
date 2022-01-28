module QueueBenchmark where

import Criterion.Main

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

emptyQ :: Queue a
emptyQ = Queue [] []

toQueue :: [a] -> Queue a
toQueue lst = go lst emptyQ
  where
    go [] q = q
    go (x : xs) q = go xs (push x q)

popLockDrop :: Int -> Queue Int
popLockDrop n =
  let z = toQueue [343, 34, 56, 7, 8, 8, 6, 7, 7, 7, 7, 44]
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

main :: IO ()
main =
  defaultMain
    [ bench "concat list" $ whnf popLockDrop 123456
    ]