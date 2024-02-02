{-# LANGUAGE Arrows #-}

module Weighted where

import Control.Arrow
import Control.Category
import Prelude hiding (id)

data a ~> b = Weighted {runWeigthed :: (a -> b), weight :: Integer}

instance Category (~>) where
  id = Weighted id 0
  Weighted f w . Weighted g w' = Weighted (f Prelude.. g) (w + w')

instance Arrow (~>) where
  arr f = Weighted f 0
  Weighted f w *** Weighted g w' = Weighted (f *** g) (w + w')

  -- or
  first (Weighted f w) = Weighted (first f) w

grindBeans :: [Bean] ~> GroundCoffee
grindBeans = Weighted undefined 10

boilWater :: Water ~> HotWater
boilWater = Weighted undefined 5

brew :: (GroundCoffee, HotWater) ~> Coffee
brew = Weighted undefined 20

putIces :: ([Ice], Coffee) ~> IcedCoffee
putIces = Weighted undefined 1

makeCoffee :: ([Bean], Water) ~> Coffee
makeCoffee = (grindBeans *** boilWater) >>> brew

freezeWater :: Water ~> [Ice]
freezeWater = Weighted undefined 20

makeIcedCoffee :: ([Bean], Water) ~> IcedCoffee
makeIcedCoffee = proc (beans, water) -> do
  ground <- grindBeans -< beans
  hotWater <- boilWater -< water
  ices <- freezeWater -< water
  coffee <- brew -< (ground, hotWater)
  putIces -< (ices, coffee)

makeIcedCoffee' :: ([Bean], Water) ~> IcedCoffee
makeIcedCoffee' = proc (beans, water) -> do
  ground <- grindBeans -< beans
  (hotWater, ices) <- (boilWater &&& freezeWater) -< water
  coffee <- brew -< (ground, hotWater)
  putIces -< (ices, coffee)

coffee :: IcedCoffee
coffee = runWeigthed makeIcedCoffee ([bean], water)

cost = weight makeIcedCoffee

instance ArrowChoice (~>)

makeCoffeeDynmically :: ([Bean], Water) ~> Either Coffee IcedCoffee
makeCoffeeDynmically = proc (beans, water) -> do
  ground <- grindBeans -< beans
  (hotWater, ices) <- (boilWater &&& freezeWater) -< water
  coffee <- brew -< (ground, hotWater)
  if length ices > 0
    then returnA -< Left coffee
    else do
      Right ^<< putIces -< (ices, coffee)

instance ArrowApply (~>)

makeCoffeeDynamically2 :: ([Bean], Water) ~> Either Coffee IcedCoffee
makeCoffeeDynamically2 = proc (beans, water) -> do
  grindBeans' <- chooseBestGrinder -< beans
  ground <- grindBeans' -< beans
  (hotWater, ices) <- (boilWater &&& freezeWater) -< water
  coffee <- brew -< (ground, hotWater)
  if length ices > 0
    then returnA -< Left coffee
    else do
      Right ^<< putIces -< (ices, coffee)

chooseBestGrinder :: [Bean] ~> ([Bean] ~> GroundCoffee)
chooseBestGrinder = undefined

bean :: Bean
bean = undefined

water :: Water
water = undefined

data Bean

data GroundCoffee

data Water

data HotWater

data Coffee

data IcedCoffee

data Ice

data Grinder