import Prelude (IO, (.), undefined, Int, id, (==), ($), Show, Eq, (+), return, Bool)
import qualified Test.QuickCheck as Q

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

data FunctorFunctor f g a = FunctorFunctor (f (g a))

instance (Functor' f, Functor' g) => Functor' (FunctorFunctor f g) where
  fmap g (FunctorFunctor ff) = FunctorFunctor ((fmap . fmap) g ff)

data Maybe a = Just a | Nothing deriving (Show, Eq)
instance (Q.Arbitrary a) => Q.Arbitrary (Maybe a) where
  arbitrary = do
    a <- Q.arbitrary
    Q.elements [Just a, Nothing]

instance Functor' Maybe where
  fmap _ (Just _) = Nothing
  fmap _ Nothing  = Nothing

newtype EvilList a = EvilList [a] deriving (Show, Eq)
instance (Q.Arbitrary a) => Q.Arbitrary (EvilList a) where
  arbitrary = do
    a <- Q.arbitrary
    return $ EvilList a

instance Functor' EvilList where
  fmap _ (EvilList []) = EvilList []
  fmap g (EvilList (x:xs)) = EvilList $ g x : g x : fmap g xs

firstFunctorLawHolds :: (Functor' f, Eq (f a)) => f a -> Bool
firstFunctorLawHolds f = fmap id f == f

secondFunctorLawHolds :: (Functor' f, Eq (f c)) => (a -> b) -> (b -> c) -> f a -> Bool
secondFunctorLawHolds h g f = (fmap g . fmap h) f == fmap (g . h) f

main :: IO ()
main = do
  Q.quickCheck $ \x -> firstFunctorLawHolds (x :: Maybe Int)
  Q.quickCheck $ \(x, i, j) -> secondFunctorLawHolds (+ i) (+ j) (x :: Maybe Int)

  Q.quickCheck $ \x -> firstFunctorLawHolds (x :: EvilList Int)
  Q.quickCheck $ \(x, i, j) -> secondFunctorLawHolds (+ i) (+ j) (x :: EvilList Int)
