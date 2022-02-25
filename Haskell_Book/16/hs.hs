{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ViewPatterns #-}

import Test.QuickCheck
import Test.QuickCheck.Function

class Sumthin a where
  s :: a -> a

class Else b where
  e :: b -> f (g a b c)

class Biffy e where
  slayer :: e a b
        -> (a -> c)
        -> (b -> d)
        -> e c d

data FixMePls a=
    FixMe 
  | Pls a 
  deriving (Eq, Show)

instance Functor FixMePls where
  fmap _ FixMe    = FixMe 
  fmap f (Pls a)  = Pls $ f a
    
data WhoCares a = 
    ItDoesnt
  | Matter a
  | WhatThisIsCalled
  deriving (Show, Eq)

instance Functor WhoCares where
  fmap _ ItDoesnt         = ItDoesnt
  fmap _ WhatThisIsCalled = WhatThisIsCalled
  fmap f (Matter a)       = Matter $ f a


--------------- Transforming the unapplied type argument

data Two a b = 
  Two a b 
  deriving (Show, Eq)

instance Functor (Two a) where
  fmap f (Two a b) = Two  a (f b)  -- a is part of the functorial structure, untouchable

data Or a b = 
    Fi a 
  | Se b
  deriving (Show, Eq)

instance Functor (Or a) where
  fmap _ (Fi a) = Fi a
  fmap f (Se b) = Se $ f b


functorIdentity :: (Functor f, Eq (f a)) => f a -> Bool
functorIdentity f =
  fmap id f == f

functorCompose :: (Eq (f c), Functor f) => (a -> b) -> (b -> c) -> f a -> Bool
functorCompose f g x =
  (fmap g (fmap f x)) == (fmap (g . f) x)


f :: [Int] -> Bool
f x = functorIdentity x

functorCompose' :: (Eq (f c), Functor f) =>
                    f a
                  -> Fun a b
                  -> Fun b c
                  -> Bool
functorCompose' x (Fun _ f) (Fun _ g) =
  (fmap (g . f) x) == (fmap g . fmap f $ x)

type IntToInt  = Fun Int Int
type IntFC = [Int] -> IntToInt -> IntToInt -> Bool

fc' = functorCompose'

yo = quickCheck (fc' :: IntFC)

--------------


incMaybe'' :: Num a => Maybe a -> Maybe a
incMaybe'' = fmap (+1)

showMaybe'' :: Show a
            => Maybe a
            -> Maybe String
showMaybe'' = fmap show

liftedInc :: (Functor f, Num a) => f a -> f a
liftedInc = fmap (+1)
x = liftedInc [1..5]


------------------

data Wrap f a = Wrap (f a) deriving (Show, Eq)

instance (Functor f) => Functor (Wrap f) where
  fmap f (Wrap fa) = Wrap $ fmap f fa 

---------------- IO Functor

getInt :: IO Int
getInt = fmap read getLine

type Nat f g = forall a . f a -> g a

-- This'll work
maybeToList :: Nat Maybe []
maybeToList Nothing = []
maybeToList (Just a) = [a]




main = do
  quickCheck f
  let tossEmOne = fmap (+1) negate
      lms       = [Nothing, Just"woohoo", Just "a"]
  print $ (fmap . fmap . fmap) replaceWithP lms
    where replaceWithP = const 'p'



