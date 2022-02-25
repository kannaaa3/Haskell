import Data.Monoid
import Test.QuickCheck

monoidAssoc :: (Eq m, Monoid m)
            => m -> m -> m -> Bool
monoidAssoc a b c =
  (a <> (b <> c)) == ((a <> b) <> c)

asc :: Eq a 
    => (a -> a -> a)
    -> a -> a -> a
    -> Bool
asc (<>) a b c =
  a <> (b <> c) == (a <> b) <> c

main = do
  verboseCheck (monoidAssoc :: String-> String-> String-> Bool)
