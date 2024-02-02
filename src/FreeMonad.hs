module Study.FreeMonad where

data Free f a = Pure a | Free (f (Free f a))

instance Functor f => Functor (Free f) where
  fmap f (Pure x) = Pure (f x)
  fmap f (Free g) = Free (fmap f <$> g)

instance Functor f => Monad (Free f) where
  return = Pure
  (>>=) :: Functor f => Free f a -> (a -> Free f b) -> Free f b
  Pure x >>= f = f x
  Free g >>= f = Free ((>>= f) <$> g)

instance Functor f => Applicative (Free f) where

someComputation :: Monad m => m Integer
someComputation = do
  x <- pure 42
  y <- pure 42
  z <- pure x
  pure (x + y + z)

someComputation' :: Monad m => m Integer
someComputation' = pure 42 >>= \x -> pure 42 >>= \y -> pure x >>= \z -> pure (x + y + z)

parallelComputation :: Monad m => a -> m (a, a)
parallelComputation x = do
  y <- f x
  z <- g x
  pure (y, z)

f :: Monad m => a -> m a
f = undefined
g :: Monad m => a -> m a
g = undefined