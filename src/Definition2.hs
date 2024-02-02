module Definition1 where

class Category (~>) where
  id :: a ~> a
  (.) :: (b ~> c) -> (a ~> b) -> (a ~> c)

class Category (~>) => Arrow (~>) where
  arr :: (a -> b) -> (a ~> b)
  (***) :: (a ~> b) -> (a' ~> b') -> ((a, a') ~> (b, b'))
  -- or
  first :: (a ~> b) -> (a, c) ~> (b, c)