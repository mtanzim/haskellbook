{-# LANGUAGE InstanceSigs #-}

module EitherT where

newtype EitherT e m a = EitherT {runEitherT :: m (Either e a)}

instance Functor m => Functor (EitherT e m) where
  fmap f (EitherT ema) = EitherT $ (fmap . fmap) f ema

instance (Applicative m) => Applicative (EitherT e m) where
  pure :: a -> EitherT e m a
  pure x = EitherT (pure (pure x))
  (<*>) :: EitherT e m (a -> b) -> EitherT e m a -> EitherT e m b
  EitherT f <*> EitherT x = EitherT $ (<*>) <$> f <*> x

instance Monad m => Monad (EitherT e m) where
  return = pure
  (>>=) :: EitherT e m a -> (a -> EitherT e m b) -> EitherT e m b
  EitherT ma >>= f = EitherT $ do
    v <- ma
    case v of
      Left x -> return $ Left x
      Right y -> runEitherT $ f y

swapEither :: Either e a -> Either a e
swapEither (Left x) = Right x
swapEither (Right x) = Left x

swapEitherT :: Functor m => EitherT e m a -> EitherT a m e
swapEitherT ema = EitherT $ swapEither <$> runEitherT ema

-- either :: (a -> c) -> (b -> c) -> Either a b -> c
-- EXAMPLES
-- * EitherT> either (+2) (const 45) (Left 23)
-- 25
-- * EitherT> either (+2) (const 45) (Right 23)
-- 45

eitherT :: Monad m => (a -> m c) -> (b -> m c) -> EitherT a m b -> m c
eitherT leftF rightG (EitherT ma) = do
  v <- ma
  case v of
    Left x -> leftF x
    Right y -> rightG y