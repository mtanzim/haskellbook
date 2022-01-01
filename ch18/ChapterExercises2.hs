module ChapterExercises2 where

import Control.Monad (ap, join, liftM2)

j :: Monad m => m (m a) -> m a
j = join

l1 :: Monad m => (a -> b) -> m a -> m b
l1 = fmap

l2 :: Monad m => (a -> b -> c) -> m a -> m b -> m c
l2 = liftM2

a :: Monad m => m a -> m (a -> b) -> m b
a = flip ap

-- TODO: come back to this
-- solution: https://github.com/andrewMacmurray/haskell-book-solutions/blob/f4fd386187c03828d1736d9a43642ab4f0ec6462/src/ch18/Exercises2.hs#L20
meh :: Monad m => [a] -> (a -> m b) -> m [b]
meh [] _ = return []
meh (cur : rest) f = liftM2 (++) (fmap (: []) (f cur)) (meh rest f)

-- come back to this
flipType :: (Monad m) => [m a] -> m [a]
flipType lst = meh lst id

-- Some hints
-- *ChapterExercises2 Control.Monad Data.Monoid> :t liftM2
-- liftM2 :: Monad m => (a1 -> a2 -> r) -> m a1 -> m a2 -> m r
-- *ChapterExercises2 Control.Monad Data.Monoid> :t (++)
-- (++) :: [a] -> [a] -> [a]
-- *ChapterExercises2 Control.Monad Data.Monoid> :t (:[])
-- (:[]) :: a -> [a]
-- *ChapterExercises2 Control.Monad Data.Monoid> fmap (:[]) [1,2,3]
-- [[1],[2],[3]]