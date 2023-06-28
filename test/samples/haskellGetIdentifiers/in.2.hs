{-# LANGUAGE InstanceSigs #-}
module G where

class C a where
  f :: a -> a

instance C Int where
  f :: Int -> Int
  f = id