{-# LANGUAGE Arrows #-}

module DynamicWeighted where

import Control.Monad.Writer (MonadWriter (..), Writer)
import Data.Monoid (Sum (..))
import Prelude hiding (id)

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
makeCoffee (beans, water) = do
  beans <- grindBeans beans
  water <- boilWater water
  brew (beans, water)

data Bean

data GroundCoffee

data Water

data HotWater

data Coffee