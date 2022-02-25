module Apl1 where
import Control.Applicative
import Data.Monoid
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

-- this isn't going to work properly

data YupList a = YupList a deriving (Eq, Show)

instance Semigroup a => Semigroup (YupList a) where
  (YupList a) <> (YupList b) = YupList (a <> b) -- not correct at all

instance Monoid a
    => Monoid (YupList a) where
  mempty = pure mempty -- not the zero but the empty
  mappend = liftA2 (<>)

instance Functor YupList where
  fmap f (YupList x) = YupList $ f x

instance Applicative YupList where
  pure x = YupList x
  

instance Arbitrary a
    => Arbitrary (YupList a) where
  arbitrary = YupList <$> arbitrary

-- instance Arbitrary a => Arbitrary (Sum a) where
--    arbitrary = Sum <$> arbitrary

instance Eq a
    => EqProp (YupList a) where
  (=-=) = eq



