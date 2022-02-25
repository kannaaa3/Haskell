module Exe where

import Test.QuickCheck
import Data.Monoid
import Control.Monad

import QcCho (monoidAssoc, monoidLeftIdentity, monoidRightIdentity)

data Optional a = 
    Nada
  | Only a
  deriving (Eq, Show)

instance (Semigroup a) => Semigroup (Optional a) where
    Nada <> x               = x
    x <> Nada               = x
    (Only x) <> (Only y)    = Only (x <> y)
    
instance Monoid a 
      => Monoid (Optional a) where
    mempty  = Nada
    mappend = (<>)

instance Arbitrary a => Arbitrary (Optional a) where
  arbitrary = do
    a <- arbitrary
    frequency [ (3, return $ Only a),
                (1, return $ Nada)]


------------------------------
newtype First' a =
  First' { getFirst' :: Optional a }
  deriving (Eq, Show)
 
instance Semigroup (First' a) where 
  First' {getFirst' = Nada} <> y    = y
  First' x <> _                     = First' x
  
instance Monoid a => Monoid (First' a) where
  mempty = First' {getFirst' = Nada}
  mappend = (<>)

-- 610
instance Arbitrary a => Arbitrary (First' a) where
   arbitrary = do
    a <- arbitrary 
    return $ First' a
    

firstMappend :: First' String
              -> First' String 
              -> First' String
firstMappend = mappend 
-- firstMappend (First' {getFirst' = Only "concho"}) (First'  {getFirst'= Only " ok"})

type FirstMappend =
     First' String
  -> First' String
  -> First' String
  -> Bool

type FstId =
  First' String -> Bool

main :: IO ()
main = do
  quickCheck (monoidAssoc :: FirstMappend)
  quickCheck (monoidLeftIdentity :: FstId)
  quickCheck (monoidRightIdentity :: FstId)


