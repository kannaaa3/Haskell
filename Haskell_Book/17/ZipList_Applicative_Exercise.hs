import Control.Applicative
import Data.Monoid
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

data List a =
  Nil
  | Cons a (List a)
  deriving (Eq, Show)

take' :: Int -> List a -> List a
take' 0 _   = Nil
take' _ Nil  = Nil
take' n (Cons a la) = Cons a $ take' (n - 1) la

append :: List a -> List a -> List a 
append Nil la' = la'
append (Cons a la) la'= Cons a $ append la la'

instance Functor List where
  fmap f Nil  = Nil
  fmap f (Cons a la) = Cons (f a) $ fmap f la

instance Applicative List where
  pure x = Cons x Nil 
  (<*>) Nil _ = Nil
  (<*>) (Cons f lf) la = fmap f la `append` (lf <*> la)

-- aint empty
instance Arbitrary a => Arbitrary (List a) where
  arbitrary = do 
    a <- arbitrary
    frequency [(3, return $ Cons a Nil),
                (4, Cons a <$> arbitrary)]

genListInt :: Gen (List Int)
genListInt = arbitrary


-- instance Arbitrary a => Arbitrary (List a) where
--   arbitrary = frequency [(3, return Nil), 
--                           (4, Cons <$> arbitrary <*> arbitrary)]



-------------------------------------------------------
newtype ZipList' a =
  ZipList' (List a)
  deriving (Eq, Show)

instance Eq a => EqProp (ZipList' a) where
  xs =-= ys = xs' `eq` ys'
    where xs' = let (ZipList' l) = xs
                in take' 3000 l
          ys' = let (ZipList' l) = ys
                in take' 3000 l



instance Functor ZipList' where
  fmap f (ZipList' xs) = ZipList' $ fmap f xs

instance Applicative ZipList' where
  pure x = ZipList' $ pure x
  (<*>) (ZipList' Nil) _ = ZipList' Nil 
  (<*>) _ (ZipList' Nil) = ZipList' Nil 
  (<*>) (ZipList' (Cons f lf)) (ZipList' (Cons a la)) = ZipList' $ Cons (f a) ls 
    where ZipList' ls = ZipList' lf <*> (ZipList' la)

infz ::  List Int
infz = Cons 1 infz

instance Arbitrary a => Arbitrary (ZipList' a) where
  arbitrary = ZipList' <$>  arbitrary

main :: IO ()
main = do 
  let zl' = ZipList'
  let z = zl' (Cons(+9)(Cons (*2) (Cons (+8) Nil )))
  --let z' = zl' infz
  
  print "The quick brown fox jumps over the lazy dog."
  -- let z' = zl' (Cons 1 (Cons 2 (Cons 3 Nil)))
  -- print $ z <*> z'
  -- ZipList' [10,4,11]
  -- print $ z <*> z'
  -- ZipList' [10,2,9] 
