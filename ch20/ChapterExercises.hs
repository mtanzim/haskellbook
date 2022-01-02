module ChapterExercises where

data Constant a b = Constant b

instance Foldable (Constant a) where
  foldMap f (Constant b) = f b

data Two a b = Two a b

instance Foldable (Two a) where
  foldMap f (Two _ b) = f b

data Three a b c = Three a b c

instance Foldable (Three a b) where
  foldMap f (Three _ _ c) = f c

data Three' a b = Three' a b b

instance Foldable (Three' a) where
  foldMap f (Three' a b b') = f b <> f b'

data Four' a b = Four' a b b b

instance Foldable (Four' a) where
  foldMap f (Four' a b b' b'') = f b <> f b' <> f b''

-- TODO: come back to this
filterF :: (Applicative f, Foldable t, Monoid (f a)) => (a -> Bool) -> t a -> f a
filterF fn = foldMap (\x -> if fn x then pure x else mempty)