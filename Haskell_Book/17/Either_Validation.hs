module Either_Validation where 

import Data.Validation

type E = Either
-- (<*>) :: f (a -> b) -> f a -> f b
-- (<*>) :: E e (a -> b) -> E e a -> E e b
-- pure :: a -> f a
-- pure :: a -> E e a

-- data Validation err a =
--     Failure err
--   | Success a
--   deriving (Eq, Show)
--   
-- validToEither :: Validation e a
--               -> Either e a
-- validToEither (Failure err) = Left err
-- validToEither (Success a) = Right a
--
-- eitherToValid :: Either e a
--               -> Validation e a
-- eitherToValid (Left err) = Failure err
-- eitherToValid (Right a) = Success a

-- eitherToValid . validToEither == id
-- validToEither . eitherToValid == id

{-
instance Applicative (Validation err) where
  pure x = Success x
  Failure e <*> _ = Failure e
  _ <*> (Failure e) = Failure e
  Success f <*> (Success y) = Success (f y)
-}


data Errors =
  DividedByZero
  | StackOverflow
  | MooglesChewedWires
  deriving (Eq, Show)

success :: (Semigroup a, Num b) => Validation a b
success = Success (+1) <*> Success 1
-- success == Success 2
failure :: Num a => Validation [Errors] a
failure = Success (+1) <*> Failure [StackOverflow]
-- failure == Failure [StackOverflow]
failure' :: Num b => Validation [Errors] ((a -> b) -> b)
failure' = Failure [StackOverflow] <*> Success (+1)
-- failure' == Failure [StackOverflow]
failures :: Validation [Errors] a
failures = Failure [MooglesChewedWires] <*> Failure [StackOverflow]
-- failures == Failure [MooglesChewedWires
--                     , StackOverflow]

----------------------------------------------------------------------------------


instance Functor (Validation e) where
  fmap _ (Failure e) = Failure e
  fmap f (Success a) = Success $ f a


instance Monoid e =>
         Applicative (Validation e) where 
  pure x = Success x
  (<*>) (Failure e) (Failure y) = Failure $ x <> y
  (<*>) (Success f) (Success a) = Success $ f a
  (<*>) (Failure e) _           = Failure e
  (<*>) _ (Failure e)           = Failure e











main = do
  print $ Right (+1) <*> Left ":("
  print "The quick brown fox jumps over the lazy dog."
