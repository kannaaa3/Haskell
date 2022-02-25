data List a =
  Nil
  | Cons a (List a)
  deriving (Eq, Show)

instance Semigroup (List a) where
  Nil <> x = x
  x <> Nil = x
  (Cons a la) <> la'  = Cons a (la <> la')

instance Monoid (List a) where
  mempty = Nil
  mappend = (<>)

instance Functor List where
  fmap _ Nil = Nil 
  fmap f (Cons a ls) = Cons (f a) (fmap f ls)

instance Applicative List where
  pure x = Cons x Nil
  (<*>) Nil _ = Nil
  (<*>) (Cons f lf) la = (fmap f la) <> (lf  <*> la)
  
