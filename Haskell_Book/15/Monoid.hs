import Control.Monad
import Test.QuickCheck
import Data.Monoid

import QcCho

data Trivial = Trivial deriving (Eq, Show)

instance Semigroup Trivial where
  (<>) _ _ = Trivial 

instance Monoid Trivial where
  mempty = Trivial
  mappend = (<>)

type TrivAssoc =
  Trivial -> Trivial -> Trivial -> Bool

instance Arbitrary Trivial where
  arbitrary = return $ Trivial


-----------
-- instance (Semigroup a, Semigroup b) => Semigroup (a,b) where
  -- (<>) (a1, b1) (a2, b2) = (a1 <> a2, b1 <> b2)

-- instance (Monoid a, Monoid b) => Monoid (a, b) where

-----------------------------------------

newtype Combine a b = 
  Combine {unCombine :: (a -> b)}

-- instance Semigroup 

instance Semigroup b => Semigroup (Combine a b) where
  Combine f1 <> Combine f2  = Combine $ f1 <> f2

instance Monoid b => Monoid (Combine a b) where
  mempty    = Combine mempty -- how
  mappend   = (<>)

----------------------------------

-- s is a state that preserved and then combined
newtype Mem s a = Mem { runMem :: a -> (s, a) }

instance Semigroup a => Semigroup (Mem s a) where
  ((Mem f1) <> (Mem f2))  = Mem {runMem = \x -> (s2, x)}
      where (s1, a1) = f1 x
            (s2, a2) = f2 a1
  

instance Monoid a => Monoid (Mem s a) where
  mempty =  Mem $ \x -> (mempty, x)
  mappend = (<>)
  


-- instance (Semigroup a, Semigroup s) => Semigroup (Mem s a) where
--   (Mem {runMem = f1}) <> (Mem {runMem = f2})    = Mem {runMem = (f1 <> f2)}
-- 
-- instance Monoid a => Monoid (Mem s a) where
--   mempty    = Mem mempty


--   mappend   = (<>)

main :: IO ()
main = do
  let sa = monoidAssoc
      mli = monoidLeftIdentity
      mlr = monoidRightIdentity
  quickCheck (sa :: TrivAssoc)
  quickCheck (mli :: Trivial -> Bool)
  quickCheck (mlr :: Trivial -> Bool)
