import Data.Monoid as Mn

-- instance Monoid b => Monoid (a -> b)
-- 
-- instance (Monoid a, Monoid b)
--         => Monoid (a, b)
-- instance (Monoid a, Monoid b, Monoid c)
--         => Monoid (a, b, c)

data Optional a = 
    Nada
  | Only a
  deriving (Eq, Show)

instance Semigroup a) => Semigroup (Optional a) where
    Nada <> x               = x
    x <> Nada               = x
    (Only x) <> (Only y)    = Only (x <> y)
    
  
instance Monoid a 
      => Monoid (Optional a) where
    mempty  = Nada
    mappend = (<>)

