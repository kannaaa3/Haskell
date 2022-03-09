module Exercises () where 

import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

import Data.Monoid
import Control.Applicative



-------------------------------------------------------------------------------------------------
------------------------------------------------------ *---------------------*
-- |Write Monad instance.|
-- *---------------------*

-- 1
data Nope a = 
  NopeDotJpg

instance Functor Nope where
  fmap _ _  = NopeDotJpg

instance Applicative Nope where
  pure _ = NopeDotJpg 
  _ <*> _ = NopeDotJpg

instance Monad Nope where
  return = pure 
  _ >>= _ = NopeDotJpg    

-------------------------------------------------------------------------------------------------

-- 2
data PhhhbbtttEither b a =
    Left' a
  | Right' b
  deriving (Eq, Show) 

instance Functor (PhhhbbtttEither b) where
  fmap f (Left' a) = Left' $ f a
  fmap _ (Right' b)  = Right' b

instance Monoid b => Applicative (PhhhbbtttEither b) where
  pure = Left' 
  (<*>) (Left' f) (Left' a) = Left' $ f a
  (<*>) (Right' b) (Right' b') = Right' $ b <> b'
  (<*>) (Right' b) _          = Right' b 
  (<*>) _ (Right' b)          = Right' b

instance (Arbitrary a, Arbitrary b) => Arbitrary (PhhhbbtttEither b a) where
  arbitrary = 
    frequency [(2, Left'  <$> arbitrary)
              ,(1, Right' <$> arbitrary)]
    -- frequency [(2, arbitrary >>= (return . Left'))
    --           ,(1, arbitrary >>= (return . Right'))]

instance Monoid b => Monad (PhhhbbtttEither b) where
  return = pure
  (Left' a) >>= f = f a
  (Right' b) >>= f = Right' b -- fail ap for monad law 

instance (Eq a, Eq b) => EqProp (PhhhbbtttEither b a) where 
  (=-=) = eq

trigger :: PhhhbbtttEither (Sum Int) (Int, String, Int)
trigger = undefined

quickCheckEither = do 
  quickBatch $ functor trigger 
  quickBatch $ applicative trigger
  quickBatch $ monad trigger
---------------------------------------------------------------------------------------------------

-- 3
newtype Identity a = Identity a
  deriving (Eq, Ord, Show)

instance Functor Identity where
  fmap f (Identity a) = Identity $ f a

instance Applicative Identity where
  pure = Identity
  Identity f <*> (Identity a) = Identity $ f a

instance Monad Identity where
  return = pure
  (Identity a) >>= f = f a

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = Identity <$> arbitrary
          -- = arbitrary >>= (return . Identity) 


instance Eq a => EqProp (Identity a) where
  (=-=) = eq

triggerIdentity :: Identity (Sum Int, String, Int)
triggerIdentity = undefined 

quickCheckIdentity = do
  quickBatch $ functor triggerIdentity
  quickBatch $ applicative triggerIdentity
  quickBatch $ monad triggerIdentity
  
---------------------------------------------------------------------------------------------------

-- 4

data List a = 
    Nil
  | Cons a (List a)
  deriving (Eq, Show)

instance Functor List where
  fmap _ Nil          = Nil
  fmap f (Cons a la)  = Cons (f a) (fmap f la)  

instance Applicative List where
  pure x            = Cons x Nil
  Nil <*> _         = Nil
  Cons f lf <*> la  = fmap f la `append` (lf <*> la)

instance Monad List where
  return = pure
  Nil >>= _         = Nil
  (Cons a la) >>= f =
    f a `append` (la >>= f)
        
append :: List a -> List a -> List a
append Nil x = x
append (Cons a la) la' = Cons a (la `append` la')

instance Arbitrary a => Arbitrary (List a) where
  arbitrary = 
    frequency [(1, return  Nil)
              ,(7, Cons <$> arbitrary <*> arbitrary)]

instance Eq a => EqProp (List a) where
  (=-=) = eq
  
quickCheckList = do
  let triggerList :: List (Sum Int, String, Int)
      triggerList = undefined
  quickBatch $ functor triggerList
  quickBatch $ applicative triggerList
  quickBatch $ monad triggerList





---------------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------------- *-----------------------------------------------*
-- |Write functions using Monad and Functor methods|
-- *-----------------------------------------------*

-- 1

j :: Monad m => m (m a) -> m a
-- j ma = ma >>= id
j = (=<<) id

-------------------------------------------------------------------------------------------------
-- 2
l1 :: Monad m => (a -> b) -> m a -> m b
l1 = (<$>)


-------------------------------------------------------------------------------------------------
-- 3
l2  ::  Monad m 
    =>  (a -> b -> c) -> m a -> m b -> m c 
l2 f ma mb = f <$> ma <*> mb


-------------------------------------------------------------------------------------------------
-- 4

a :: Monad m => m a -> m (a -> b) -> m b
a = flip (<*>)

-------------------------------------------------------------------------------------------------
-- 5
meh ::  Monad m
    =>  [a] -> (a -> m b) -> m [b]
meh [] _ = pure []
meh (x : xs) f = 
  flipCons <$> meh xs f <*> f x
    where flipCons = flip (:)

-------------------------------------------------------------------------------------------------
-- 6
flipType  :: (Monad m) => [m a] -> m [a]
flipType  =  flip meh id
-- [m a] -> (m a -> m b) -> m [b]

