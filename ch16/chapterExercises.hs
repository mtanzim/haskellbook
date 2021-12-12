{-# LANGUAGE FlexibleInstances #-}

module ChapterExercises where

import GHC.Arr

newtype Mu f = InF {outF :: f (Mu f)}

-- Possible to define Functor since,  Mu :: (* -> *) -> *

data D = D (Array Word Word) Int Int

-- Not possible to define Functor since, D :: *

data Sum a b = First a | Second b

instance Functor (Sum a) where
  fmap f (First a) = First a
  fmap f (Second b) = Second (f b)

data Company a b c = DeepBlue a c | Something b

instance Functor (Company a b) where
  fmap _ (Something b) = Something b
  fmap f (DeepBlue a c) = DeepBlue a (f c)

data More a b = L a b a | R b a b deriving (Eq, Show)

newtype Flip f a b = Flip (f b a) deriving (Eq, Show)

instance Functor (Flip More a) where
  fmap f (Flip (L a b a')) = Flip $ L (f a) b (f a')
  fmap f (Flip (R b a b')) = Flip $ R b (f a) b'

data Quant a b = Finance | Desk a | Bloor b

instance Functor (Quant a) where
  fmap _ Finance = Finance
  fmap _ (Desk a) = Desk a
  fmap f (Bloor b) = Bloor (f b)

-- data K a b = K a

instance Functor (K a) where
  fmap _ (K a) = K a

newtype K a b = K a

instance Functor (Flip K a) where
  fmap f (Flip (K a)) = Flip $ K (f a)

data EvilGoateeConst a b = GoatyConst b deriving (Eq, Show)

instance Functor (EvilGoateeConst a) where
  fmap f (GoatyConst b) = GoatyConst (f b)

data LiftItOut f a = LiftItOut (f a) deriving (Eq, Show)

instance Functor f => Functor (LiftItOut f) where
  fmap f (LiftItOut ga) = LiftItOut (fmap f ga)

data Parappa f g a = Dawrappa (f a) (g a) deriving (Eq, Show)

instance (Functor f, Functor g) => Functor (Parappa f g) where
  fmap f (Dawrappa fa ga) = Dawrappa (fmap f fa) (fmap f ga)

data IgnoreOne f g a b = IgnoringSomething (f a) (g b)

instance Functor g => Functor (IgnoreOne f g a) where
  fmap f (IgnoringSomething fa gb) = IgnoringSomething fa (fmap f gb)

data Notorious g o a t = Notorious (g o) (g a) (g t)

instance Functor g => Functor (Notorious g o a) where
  fmap f (Notorious go ga gt) = Notorious go ga (fmap f gt)

data List a = Nil | Cons a (List a)

instance Functor List where
  fmap _ Nil = Nil
  fmap f (Cons a rest) = Cons (f a) (fmap f rest)

data GoatLord a
  = NoGoat
  | OneGoat a
  | MoreGoats (GoatLord a) (GoatLord a) (GoatLord a)

instance Functor (GoatLord) where
  fmap _ NoGoat = NoGoat
  fmap f (OneGoat a) = OneGoat (f a)
  fmap f (MoreGoats gla glb glc) = MoreGoats (fmap f gla) (fmap f glb) (fmap f glc)

data TalkToMe a = Halt | Print String a | Read (String -> a)

instance Functor (TalkToMe) where
  fmap _ Halt = Halt
  fmap f (Print s a) = Print s (f a)
  fmap f (Read sa) = Read (fmap f sa)