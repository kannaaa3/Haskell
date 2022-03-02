data Sum a b =
    First a
  | Second b
  deriving (Eq, Show)

instance Functor (Sum a) where
  fmap _ (First a)   = First a
  fmap f (Second b)  = Second $ f b


-- Sum a (a1 -> b) -> Sum a a1 -> Sum a b
instance Monoid a => Applicative (Sum a) where
  pure = Second
  First x <*> (First y) = First $ x <> y
  Second f <*> (Second x) = Second $ f x
  First f <*> _           = First f
  _ <*> (First x)        = First x

instance Monoid a => Monad (Sum a) where
  return = pure
  -- Sum e a -> (a -> Sum e b) -> Sum e b
  (>>=) (Second b) f = f b 
  (>>=) (First a) _ = First a
