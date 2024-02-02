{-# LANGUAGE Arrows #-}

module Study.DynamicWeighted where

import Prelude hiding (id)
import Control.Monad.Writer (Writer, MonadWriter (..))
import Data.Monoid (Sum(..))

type a ~> b = a -> Writer (Sum Int) b
  
grindBeans :: [Bean] ~> GroundCoffee
grindBeans beans = do
  tell (Sum $ 3 * length beans)
  pure undefined

boilWater :: Water ~> HotWater
boilWater water = undefined

brew :: (GroundCoffee, HotWater) ~> Coffee
brew = undefined

makeCoffee :: ([Bean], Water) ~> Coffee
makeCoffee (beans, water)= do
  beans <- grindBeans beans
  water <- boilWater water
  brew (beans, water)


data Bean
data GroundCoffee
data Water
data HotWater
data Coffee