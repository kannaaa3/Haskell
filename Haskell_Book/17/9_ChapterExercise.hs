import Control.Applicative (liftA3)
-- 740

-- (,) a
-- pure :: a -> (a, a)
-- (<*>) :: (a, (a -> b)) -> (a, a) -> (a, b)

 -- Type (->) e
-- Methods
-- pure :: a -> e -> a
-- (<*>) :: (e -> a -> b) -> (e -> a) -> (e -> b) 


data Three a b c = Three a b c deriving (Eq, Show)

instance Functor (Three a b) where
  fmap f (Three a b x) = Three a b (f x)

instance (Monoid a, Monoid b) => Applicative (Three a b) where
  pure x = Three mempty mempty x
  (Three a1 b1 f) <*> (Three a2 b2 x) = Three (a1 <> a2) (b1 <> b2) $ f x

-------------------------------------------------------------------------------------------------
data Four' a b = Four' a a a b deriving (Eq, Show)

instance Functor (Four' a) where
  fmap f (Four' a1 a2 a3 x) = Four' a1 a2 a3 $ f x

instance Monoid a => Applicative (Four' a) where
  pure x = Four' mempty mempty mempty x
  (Four' a1 a2 a3 f) <*> (Four' a1' a2' a3' x) = Four' (a1 <> a1') (a2 <> a2') (a3 <> a3') $ f x

-------------------------------------------------------------------------------------------------

stops :: String
stops = "pbtdkg"

vowels :: String
vowels = "aeiou"

combos :: [a] -> [b] -> [c] -> [(a, b, c)]
combos = liftA3 (,,)
--  [(3,6,"cho"),(3,6,"gau"),(3,3,"cho"),(3,3,"gau"),(4,6,"cho"),(4,6,"gau"),(4,3,"cho"),(4,3,"gau")]
