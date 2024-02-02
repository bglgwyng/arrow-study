{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecursiveDo #-}

module AutomataS2 where

import Control.Arrow
import Control.Category
import FreeArrow (someComputation)
import Prelude hiding (id, (.))

data a ~> b where
  AutomataS :: s -> (s -> a -> (s, b)) -> a ~> b
  Id :: a ~> a

instance Category (~>) where
  id :: a ~> a
  id = AutomataS () (\_ x -> ((), x))
  (.) :: b ~> c -> a ~> b -> a ~> c
  AutomataS fs f . AutomataS gs g =
    AutomataS
      (fs, gs)
      ( \(fs, gs) x ->
          let (gs', y) = g gs x
              (fs', z) = f fs y
           in ((fs', gs'), z)
      )
  a@(AutomataS {}) . Id = a
  Id . a@(AutomataS {}) = a
  Id . Id = Id

instance Arrow (~>) where
  arr :: (b -> c) -> b ~> c
  arr f = AutomataS () (\_ x -> ((), f x))
  (***) :: (b ~> c) -> (b' ~> c') -> (b, b') ~> (c, c')
  AutomataS fs f *** AutomataS gs g =
    AutomataS
      (fs, gs)
      ( \(fs, gs) (x, x') ->
          let (s', y) = f fs x
              (s'', y') = g gs x'
           in ((s', s''), (y, y'))
      )
  AutomataS fs f *** Id = AutomataS fs (\fs (x, y) -> let (s', x') = f fs x in (s', (x', y)))
  Id *** AutomataS gs g = AutomataS gs (\gs (x, y) -> let (s', y') = g gs y in (s', (x, y')))
  Id *** Id = Id

instance ArrowChoice (~>)

someComputation :: a ~> a
someComputation = proc x -> do
  y <- id -< x
  z <- id -< y
  id -< z

someComputation' :: a ~> a
someComputation' = proc x -> do
  y <-
    if someCondition x
      then do
        f -< x
      else id -< x
  z <- id -< y
  id -< z

parallelComputation :: a ~> (a, a)
parallelComputation = proc x -> do
  y <- f -< x
  z <- g -< x
  id -< (y, z)

someCondition :: a -> Bool
someCondition = undefined

f :: (Arrow a) => a b b
f = undefined

g :: (Arrow a) => a b b
g = undefined
