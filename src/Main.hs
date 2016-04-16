import Prelude (IO, (.), undefined, Int)

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

data ITree a = Leaf (Int -> a) | Node [ITree a]

instance Functor' [] where
  fmap _ []     = []
  fmap g (x:xs) = g x : fmap g xs

instance Functor' ITree where
  fmap g (Leaf h)  = Leaf (fmap g h)
  fmap g (Node as) = Node ((fmap . fmap) g as)

data NoFunctor a = NoFunctor (a -> Int)

instance Functor' NoFunctor where
  fmap g (NoFunctor h) = NoFunctor (undefined g h)

main :: IO ()
main = undefined
