module ValidationApplicative where

data Validation e a = Failure e | Success a deriving (Eq, Show)

instance Functor (Validation e) where
    fmap _ (Failure e) = Failure e
    fmap f (Success a) = Success (f a)

instance Monoid e => Applicative (Validation e) where
    pure a = Success a
    Failure e <*> Success _ = Failure e
    Success _ <*> Failure e = Failure e
    Success a <*> Success a' = Success (a a')
    Failure e <*> Failure e' = Failure (e <> e')