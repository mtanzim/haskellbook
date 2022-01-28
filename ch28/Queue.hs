data Queue a = Queue
  { enqueue :: [a],
    dequeue :: [a]
  }
  deriving (Eq, Show)

push :: a -> Queue a -> Queue a
push a (Queue curEq curDq) =
  let updatedQ = a : curEq
   in Queue updatedQ (reverse updatedQ)

pop :: Queue a -> Maybe (a, Queue a)
pop (Queue [] []) = Nothing
pop (Queue [a] [_]) = Just (a, Queue [] [])
pop (Queue curEq curDq) =
  let updatedQ = tail curDq
   in Just (head curDq, Queue (reverse updatedQ) updatedQ)

emptyQ :: Queue a
emptyQ = Queue [] []