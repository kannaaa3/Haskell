module QcCho (
monoidLeftIdentity,
monoidRightIdentity,
monoidAssoc
)  where

import Control.Monad
import Data.Monoid
import Test.QuickCheck


monoidLeftIdentity :: (Eq m, Monoid m) => m -> Bool
monoidLeftIdentity a = (mempty <> a) == a

monoidRightIdentity :: (Eq m, Monoid m) => m -> Bool
monoidRightIdentity a = (a <> mempty) == a

monoidAssoc :: (Eq m, Monoid m) => m -> m -> m -> Bool
monoidAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)

{-
main :: IO ()
main = do
  let ma    = monoidAssoc
      mli   = monoidLeftIdentity
      mlr   = monoidRightIdentity
  quickCheck (ma :: BullMappend)
  quickCheck (mli :: Bull -> Bool)
  quickCheck (mlr :: Bull -> Bool)
  -}
