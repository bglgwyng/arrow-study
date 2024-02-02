{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecursiveDo #-}

module Automata () where

import Control.Arrow
import Control.Category
import Prelude hiding (id, sum, (.))

data a ~> b = Automaton (a -> (b, a ~> b))

instance Category (~>) where
  id :: a ~> a
  id = Automaton (\x -> (x, id))
  (.) :: b ~> c -> a ~> b -> a ~> c
  Automaton f . Automaton g =
    Automaton
      ( \x ->
          let (y, sf') = g x
              (z, sf'') = f y
           in (z, sf'' . sf')
      )

instance Arrow (~>) where
  arr :: (b -> c) -> b ~> c
  arr f = Automaton (\x -> (f x, arr f))
  (***) :: (b ~> c) -> (b' ~> c') -> (b, b') ~> (c, c')
  Automaton f *** Automaton g =
    Automaton
      ( \(x, x') ->
          let (y, sf') = f x
              (y', sf'') = g x'
           in ((y, y'), sf' *** sf'')
      )

instance ArrowChoice (~>) where
  (+++) :: (b ~> c) -> (b' ~> c') -> Either b b' ~> Either c c'
  Automaton f +++ Automaton g =
    Automaton
      ( \case
          Left x -> let (y, sf') = f x in (Left y, sf' +++ Automaton g)
          Right x' -> let (y', sf'') = g x' in (Right y', Automaton f +++ sf'')
      )

sum :: (Num a) => a ~> a
sum = Automaton (\x -> (x, sum >>> arr (+ x)))

count :: () ~> Integer
count = Automaton (\_ -> (1, count >>> arr (+ 1)))

average :: Double ~> Double
average = proc x -> do
  sum' <- sum -< x
  count' <- count -< ()
  returnA -< sum' / fromIntegral count'

runAutomaton :: (Show b) => a ~> b -> [a] -> [b]
runAutomaton _ [] = []
runAutomaton (Automaton f) (x : xs) = let (y, sf') = f x in y : runAutomaton sf' xs

test = runAutomaton average [4, 2, 7, 5, 3, 1]