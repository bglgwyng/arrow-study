{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE LambdaCase #-}
module Study.AutomataS where

import Prelude hiding ((.), id)
import Control.Category
import Control.Arrow

data a ~> b = 
  forall s. AutomataS s (s -> a -> (s, b))

instance Category (~>) where
  id :: a ~> a
  id = AutomataS () (\_ x -> ((), x))
  (.) :: b ~> c -> a ~> b -> a ~> c
  AutomataS fs f . AutomataS gs g = 
    AutomataS 
      (fs, gs) 
      (\(fs, gs) x -> 
        let (gs', y) = g gs x
            (fs', z) = f fs y
        in ((fs', gs'), z))

instance Arrow (~>) where
  arr :: (b -> c) -> b ~> c
  arr f = AutomataS () (\_ x -> ((), f x))  
  (***) :: (b ~> c) -> (b' ~> c') -> (b, b') ~> (c, c')
  AutomataS fs f *** AutomataS gs g = 
    AutomataS 
      (fs, gs) 
      (\(fs, gs) (x, x') -> 
        let (s', y) = f fs x
            (s'', y') = g gs x'
        in ((s', s''), (y, y'))) 


instance ArrowChoice (~>) where
  (+++) :: (b ~> c) -> (b' ~> c') -> Either b b' ~> Either c c'
  AutomataS fs f +++ AutomataS gs g = 
    AutomataS 
      (fs, gs) 
      (\(fs, gs) -> 
        \case
          Left x -> let (s', y) = f fs x in ((s', gs), Left y)
          Right x' -> let (s', y') = g gs x' in ((fs, s'), Right y'))


count' :: () ~> Int
count' = AutomataS 0 (\s _ -> (s + 1, s + 1))

scan :: s -> (s -> a -> s) -> a ~> s
scan initial_s f = AutomataS initial_s (\s x -> (f s x, f s x))

sum' :: Int ~> Int
sum' = scan 0 (+)

average :: Int ~> Int
average = proc x -> do
  sum <- sum' -< x
  count <- count' -< ()
  returnA -< sum `div` count

runAutomataS :: Show b => a ~> b -> [a] -> [b]
runAutomataS _ [] = []
runAutomataS (AutomataS s f) (x:xs) = let (s', y) = f s x in y : runAutomataS (AutomataS s' f) xs
