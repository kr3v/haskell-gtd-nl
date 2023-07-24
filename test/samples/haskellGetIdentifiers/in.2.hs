{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TypeOperators #-}

module G where

class C a where
  f :: a -> a

instance C Int where
  f :: Int -> Int
  f = id

class Applicative m => Monad m where
    (>>=)       :: forall a b. m a -> (a -> m b) -> m b

    (>>)        :: forall a b. m a -> m b -> m b
    m >> k = m >>= \_ -> k
    {-# INLINE (>>) #-}

    return      :: a -> m a
    return      = pure