module ChapterExercises where

data Pair a = Pair a a deriving (Eq, Show)

-- instance Semigroup a => Semigroup (Pair a) where
--   Pair x x' <> Pair y y' = Pair (x <> x') (y <> y')

-- instance Monoid a => Monoid (Pair a) where
--   mappend = (<>)
--   mempty = Pair mempty mempty

instance Functor Pair where
  fmap f (Pair a a') = Pair (f a) (f a')

instance Applicative Pair where
  pure a = Pair a a
  Pair f f' <*> Pair x x' = Pair (f x) (f' x')
