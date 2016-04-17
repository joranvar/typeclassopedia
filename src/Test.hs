import Prelude (IO, (.), undefined, Int, id, (==), ($), Show, Eq, (+), return, Bool, const, uncurry)
import qualified Test.QuickCheck as Q
import Data.List (repeat, zipWith)

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

functorLaws :: IO ()
functorLaws = do
  Q.quickCheck $ \x -> firstFunctorLawHolds (x :: Maybe Int)
  Q.quickCheck $ \(x, i, j) -> secondFunctorLawHolds (+ i) (+ j) (x :: Maybe Int)

  Q.quickCheck $ \x -> firstFunctorLawHolds (x :: EvilList Int)
  Q.quickCheck $ \(x, i, j) -> secondFunctorLawHolds (+ i) (+ j) (x :: EvilList Int)

-- pure f <*> x = pure (flip ($)) <*> x <*> pure f
-- Interchange:
-- pure f <*> x = pure (flip ($)) <*> pure ($ f) <*> x
-- Homomorphism:
-- pure f <*> x = pure (flip ($) ($ f)) <*> x
-- Simplification of (flip ($) ($ f))
-- pure f <*> x = pure (flip ($) ($ f)) <*> x

class (Functor' f) => Applicative' f where
  pure  :: a -> f a
  infixl 4 <*>
  (<*>) :: f (a -> b) -> f a -> f b

instance Applicative' Maybe where
  pure              = Just
  Just g <*> Just x = Just $ g x
  _      <*> _      = Nothing

newtype ZipList a = ZipList { getZipList :: [a] } deriving (Show, Eq)

instance (Q.Arbitrary a) => Q.Arbitrary (ZipList a) where
  arbitrary = do
    a <- Q.arbitrary
    return $ ZipList a

instance Functor' ZipList where
  fmap g = ZipList . fmap g . getZipList

instance Applicative' ZipList where
  pure = ZipList . repeat
  (ZipList gs) <*> (ZipList xs) = ZipList (zipWith ($) gs xs)

class (Functor' f) => Monoidal' f where
  unit :: f ()
  (**) :: f a -> f b -> f (a, b)

newtype MonoidalApplicative f a = MonoidalApplicative { getMonoidalApplicative :: f a }

instance (Functor' f) => Functor' (MonoidalApplicative f) where
  fmap g = MonoidalApplicative . fmap g . getMonoidalApplicative

instance (Applicative' f) => Monoidal' (MonoidalApplicative f) where
  unit   = MonoidalApplicative $ pure ()
  x ** y = MonoidalApplicative $ pure (,) <*> getMonoidalApplicative x <*> getMonoidalApplicative y

instance (Monoidal' f) => Applicative' (MonoidalApplicative f) where
  pure a  = MonoidalApplicative $ fmap (const a) unit
  g <*> x = MonoidalApplicative $ fmap (uncurry ($)) $ getMonoidalApplicative g ** getMonoidalApplicative x

firstApplicativeLawHolds :: (Applicative' a, Eq (a b)) => a b -> Bool
firstApplicativeLawHolds a = (pure id <*> a) == a

applicativeLaws :: IO ()
applicativeLaws = do
  Q.quickCheck $ \x -> firstApplicativeLawHolds (x :: Maybe Int)
  Q.quickCheck $ \x -> firstApplicativeLawHolds (x :: ZipList Int)

main :: IO ()
main = do
  functorLaws
  applicativeLaws
