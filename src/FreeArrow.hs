{-# LANGUAGE Arrows #-}

module FreeArrow where

import Control.Arrow
import Control.Category
import Prelude hiding (id)

data Free eff a b where
  Pure :: (a -> b) -> Free eff a b
  Effect :: eff a b -> Free eff a b
  Seq :: Free eff a b -> Free eff b c -> Free eff a c
  Par :: Free eff a1 b1 -> Free eff a2 b2 -> Free eff (a1, a2) (b1, b2)

instance Category (Free eff) where
  id = Pure id
  (.) = flip Seq

instance Arrow (Free eff) where
  arr = Pure
  (***) = Par

someComputation :: (Arrow a) => a () Int
someComputation = proc _ -> do
  x <- id -< 42
  y <- id -< 42
  z <- id -< y
  id -< (x + y + z)

someComputation' :: (Arrow a) => a () Int
someComputation' =
  (arr (\_ -> 42) &&& arr (\_ -> 42))
    >>> (id *** arr (\y -> (y, y)))
    >>> arr (\(x, (y, z)) -> x + y + z)