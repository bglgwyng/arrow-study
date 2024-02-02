{-# LANGUAGE Arrows #-}

module Study.Kleisli where
import Control.Arrow (Kleisli(..), (***))
import Control.Monad.Writer
import Data.Monoid
import Control.Category ((>>>))

type DynamicWeighted =  Kleisli (Writer (Sum Int))
type a ~> b = DynamicWeighted a b


grindBeans :: [Bean] ~> GroundCoffee
grindBeans = Kleisli $ \beans -> do
  tell (Sum $ 3 * length beans)
  pure undefined

boilWater :: Water ~> HotWater
boilWater = undefined

brew :: (GroundCoffee, HotWater) ~> Coffee
brew = undefined

makeCoffee :: ([Bean], Water) ~> Coffee
makeCoffee = (grindBeans *** boilWater) >>> brew

makeCoffee' :: ([Bean], Water) ~> Coffee
makeCoffee' = proc (beans, water) -> do
  ground <- grindBeans -< beans
  hotWater <- boilWater -< water
  brew -< (ground, hotWater)

data Bean
data GroundCoffee
data Water
data HotWater
data Coffee