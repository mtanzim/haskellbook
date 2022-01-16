class Bifunctor p where
  bimap :: (a -> b) -> (c -> d) -> p a c -> p b d
  bimap f g = first f . second g
  first :: (a -> b) -> p a c -> p b c
  first f = bimap f id
  second :: (b -> c) -> p a b -> p a c
  second g = bimap id g

data Deux a b = Deux a b

instance Bifunctor Deux where
  bimap f g (Deux a b) = Deux (f a) (g b)
  first f (Deux a b) = Deux (f a) b
  second g (Deux a b) = Deux a (g b)

data Const a b = Const a

instance Bifunctor Const where
  bimap f _ (Const a) = Const (f a)
  first f (Const a) = Const (f a)
  second _ (Const a) = Const a

data Drei a b c = Drei a b c

instance Bifunctor (Drei a) where
  bimap f g (Drei a b c) = Drei a (f b) (g c)
  first f (Drei a b c) = Drei a (f b) c
  second g (Drei a b c) = Drei a b (g c)

data SuperDrei a b c = SuperDrei a b

instance Bifunctor (SuperDrei a) where
  bimap f _ (SuperDrei a b) = SuperDrei a (f b)
  first f (SuperDrei a b) = SuperDrei a (f b)
  second _ (SuperDrei a b) = SuperDrei a b

data SemiDrie a b c = SemiDrie a

instance Bifunctor (SemiDrie a) where
  bimap _ _ (SemiDrie a) = SemiDrie a
  first _ (SemiDrie a) = SemiDrie a
  second _ (SemiDrie a) = SemiDrie a

data Either' a b = Left' a | Right' b

instance Bifunctor Either' where
  bimap f g (Left' a) = Left' (f a)
  bimap f g (Right' b) = Right' (g b)
  first f (Left' a) = Left' (f a)
  first _ (Right' b) = Right' b
  second _ (Left' a) = Left' a
  second g (Right' b) = Right' (g b)
