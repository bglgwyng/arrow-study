module Definition1 where

class Category a where
  id :: a b b
  (.) :: a c d -> a b c -> a b d

class Category a => Arrow a where
  arr :: (b -> c) -> a b c
  (***) :: a b c -> a b' c' -> a (b, b') (c, c')