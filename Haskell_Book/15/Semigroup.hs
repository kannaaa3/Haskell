import Test.QuickCheck
import Control.Monad
import Data.Monoid

------------
semigroupAssoc :: (Eq m, Semigroup m)
                => m -> m -> m -> Bool
semigroupAssoc a b c =
  (a <> (b <> c)) == ((a <> b) <> c)

----------------------------------------------------
data Trivial = Trivial deriving (Eq, Show)

instance Semigroup Trivial where
  _ <> _ =  Trivial 

instance Arbitrary Trivial where
  arbitrary = return Trivial

type TrivAssoc =
  Trivial -> Trivial -> Trivial -> Bool


-----------------------------------------------------

newtype Identity a = Identity a deriving (Eq, Show)

instance (Semigroup a) => Semigroup (Identity a) where
  (Identity x) <> (Identity y)  = Identity $ x <> y

instance (Arbitrary a) => Arbitrary (Identity a) where
  arbitrary = do
    a <- arbitrary
    return $ Identity a

type IdenAssoc a = 
  Identity a -> Identity a -> Identity a -> Bool


----------------------------------------------------

data Two a b = Two a b deriving (Eq, Show)

instance (Semigroup a, Semigroup b) => Semigroup (Two a b) where
  (Two a1 b1) <> (Two a2 b2) = Two (a1 <> a2) (b1 <> b2)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    return $ Two a b

type TwoAssoc a b =
  Two a b ->Two a b ->Two a b -> Bool

--------------------------------------------

newtype BoolConj =
  BoolConj Bool 
  deriving (Show, Eq)

instance Semigroup BoolConj where
  (BoolConj True) <> (BoolConj True)    = BoolConj True
  _ <> _                                = BoolConj False
------------------------------------------

data Or a b =
    Fst a
  | Snd b
  deriving (Show, Eq)

instance Semigroup (Or a b) where
  (Snd x) <> _  = Snd x
  _ <> (Snd x)  = Snd x
  x <> _        = x
-----------------------------------------








newtype Combine a b =
  Combine { unCombine :: (a -> b) }

--instance (Semigroup b) => Semigroup (a -> b) where
--  (f1 <> f2) x = (f1 x) <> (f2 x)

instance Semigroup b => Semigroup (Combine a b) where
  (Combine f1) <> (Combine f2)  =  Combine $ f1 <> f2

-----------------------------
--

data Validation' a b =
    Failure' a
  | Success' b
  deriving (Show, Eq)

instance Semigroup (Validation' a b) where
  (Success' x) <> _  = Success' x
  _ <> (Success' x)  = Success' x
  x <> _        = x

newtype AccumulateRight a b =
  AccumulateRight (Validation' a b)
  deriving (Eq, Show)

instance Semigroup b =>
  Semigroup (AccumulateRight a b) where
  (<>) (AccumulateRight x) (AccumulateRight y)  = AccumulateRight $ x <> y






main :: IO ()
main =
  quickCheck (semigroupAssoc :: TrivAssoc)
