import Data.Foldable 
import Data.Monoid 

xs :: Product Integer
xs = foldMap Product [1..5]
    -- fold $ map Product [1..5]

ys :: Product Integer 
ys = foldMap ((*5) . Product) [1..3]

x1 :: Product Integer 
x1 = foldMap (*5) (Just 3) -- With just 1 value, no need Monoid
-- Otherwise, mempty 
x2 :: Sum Integer 
x2 = foldMap (*5) Nothing 
  -- Sum {getSum = 0}

-------------------------------------------------------------------------------------------------
---------------------------- *--------*
-- |Identity|
-- *--------*

newtype Identity a = 
  Identity a deriving (Eq, Show)

instance Foldable Identity where
  foldr f z (Identity x)  = f x z
  foldl f z (Identity x)  = f z x 
  foldMap f (Identity x)  = f x 


-------------------------------------------------------------------------------------------------
---------------------- *-----*
-- |Maybe|
-- *-----*

data Optional a = 
    Nada 
  | Yep a 
  deriving (Eq, Show)

instance Foldable Optional where
  foldr _ z Nada = z
  foldr f z (Yep x) = f x z

  foldl _ z Nada = z
  foldl f z (Yep x) = f z x

  foldMap _ Nada = mempty
  foldMap f (Yep a) = f a




-------------------------------------------------------------------------------------------------
-- concatMap :: (a -> [b]) -> t a -> [b]
xS :: Num a => [a]
xS = concatMap toList [Just 1, Just 2, Just 3]
--   [1,2,3]
-- toList (1,2) [2]


-------------------------------------------------------------------------------------------------
-- *-----------------------------------*
-- |Test whether the structure is empty|
-- *-----------------------------------*
-- null :: t a -> Bool
xNull :: [Bool]
xNull = fmap null [Just 1, Just 2, Nothing]

-------------------------------------------------------------------------------------------------

-- *-------------------------------------------------*
-- |Returns the size of a finit structure as an 'Int'|
-- *-------------------------------------------------*
-- length :: t a -> Int

xLength :: [Int]
xLength = fmap length [Just 1, Just 2, Nothing]
      --  [1,1,0]

-------------------------------------------------------------------------------------------------
-- *---------------------------------------*
-- |Does the element occur in the structure|
-- *---------------------------------------*
-- elem :: Eq a => a -> t a -> Bool

xElem :: [Bool]
xElem = fmap (elem 3) [Left 3, Right 3]

-- maximum :: Ord a => t a -> a 
-- minimum :: Ord a => t a -> a 

xMinimum :: String
xMinimum = fmap minimum (map Just "jul")
        -- "jul"
--sum, product 



