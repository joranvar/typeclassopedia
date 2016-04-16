import Prelude (IO, (.), undefined)

class Functor' f where
  fmap :: (a -> b) -> f a -> f b

data Either e a = Left e | Right a

instance Functor' (Either e) where
  fmap _ (Left e)  = Left e
  fmap g (Right a) = Right (g a)

instance Functor' ((->) e) where
  fmap = (.)

instance Functor' ((,) e) where
  fmap g (e, a) = (e, g a)

data Pair a = Pair a a

instance Functor' Pair where
  fmap g (Pair a b) = Pair (g a) (g b)

main :: IO ()
main = undefined
