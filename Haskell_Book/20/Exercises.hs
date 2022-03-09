import Data.Monoid 
import Data.Foldable


-- *-----------------*
-- |library functions|
-- *-----------------*

sum' :: (Foldable t, Num a) => t a -> a 
sum'  = foldr (+) 0

elem' :: (Foldable t, Eq a)
     => a -> t a -> Bool
elem' x = foldr (\a acc -> (x == a) || acc) False

maximum' :: (Foldable t, Ord a)
         => t a -> Maybe a
maximum' = foldr f Nothing
  where f a acc =  case acc of 
          Nothing -> Just a 
          Just a' -> Just $ max a a'

null' :: (Foldable t) => t a -> Bool 
null' = foldr f True
  where f _ _ = False

toList' :: (Foldable t) => t a -> [a]
toList' =  foldr (:) [] 

foldMe :: (Foldable t, Monoid m)
      => t m -> m
foldMe = foldMap id 

foldMapMe :: (Foldable t, Monoid m)
          => (a -> m) -> t a -> m 
foldMapMe g  = foldr f mempty 
  where f a acc = g a <> acc

-------------------------------------------------------------------------------------------------
    
newtype Constant a b = 
  Constant b 
  deriving (Eq, Show)

instance Foldable (Constant a) where
  foldMap f (Constant b)      = f b 
  foldr f acc (Constant b)    = f b acc
  foldl f acc (Constant b)    = f acc b

-------------------------------------------------------------------------------------------------
data Two a b = 
  Two a b 
  deriving (Eq, Show)

instance Foldable (Two a) where
  foldMap f (Two a b) = f b 
  foldr f acc (Two a b) = f b acc
  foldl f acc (Two a b) = f acc b

-------------------------------------------------------------------------------------------------
data Four' a b = 
  Four' a b b b 
  deriving (Eq, Show)

instance Foldable (Four' a) where
  foldMap f (Four' a b1 b2 b3) = f b1 <> f b2 <> f b3
  foldr f acc (Four' a b1 b2 b3) = f b3 (f b2 (f b1 acc))
  foldl f acc (Four' a b1 b2 b3) = f (f (f acc b1) b2) b3

-------------------------------------------------------------------------------------------------

filterF :: (Applicative f,
            Foldable t,
            Monoid (f a))
        => (a -> Bool) -> t a -> f a
filterF check = foldMap f 
  where f a = 
          case check a of 
             True -> pure a 
             _    -> mempty


