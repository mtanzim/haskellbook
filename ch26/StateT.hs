module StateT where

newtype StateT s m a = StateT {runStateT :: s -> m (a, s)}

instance (Monad m) => Functor (StateT s m) where
  fmap f (StateT sma) = StateT $ \s -> do
    (a, _) <- sma s
    return (f a, s)

instance (Monad m) => Applicative (StateT s m) where
  pure x = StateT $ \s -> return (x, s)
  StateT smaf <*> StateT sma = StateT $ \s -> do
    (a, _) <- sma s
    (f, _) <- smaf s
    return (f a, s)

instance (Monad m) => Monad (StateT s m) where
  return = pure
  StateT sma >>= f =
    StateT $ \s -> do
      (a, _) <- sma s
      runStateT (f a) s