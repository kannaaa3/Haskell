import Data.List (elemIndex)
import Data.Monoid
  
added :: Maybe Integer
added =
   (+3) <$> (lookup 3 $ zip [1, 2, 3] [4, 5, 6])

-- y :: Maybe Integer
-- y = lookup 3 $ zip [1, 2, 3] [4, 5, 6]
--
-- z :: Maybe Integer 
-- z = lookup 2 $ zip [1, 2, 3] [4, 5, 6]
--
-- tupled :: Maybe (Integer, Integer)
-- tupled = (,) <$> y <*> z

--------------------------------------
-- x :: Maybe Int
-- x = elemIndex 3 [1, 2, 3, 4, 5]
--
-- y :: Maybe Int
-- y = elemIndex 4 [1, 2, 3, 4, 5]
--
-- max' :: Int -> Int -> Int
-- max' = max
--
-- maxed :: Maybe Int
-- maxed = max' <$> x <*> y
---------------------------------
xs = [1, 2, 3]
ys = [4, 5, 6]

x :: Maybe Integer
x = lookup 3 $ zip xs ys

y :: Maybe Integer
y = lookup 2 $ zip xs ys

summed :: Maybe Integer
summed = sum <$> ((,) <$> x <*> y)


newtype Identity a = Identity a
  deriving (Eq, Ord, Show)

instance Functor Identity where
  fmap f (Identity a) = Identity $ f a

instance Applicative Identity where
  pure = Identity
  (<*>) (Identity f) (Identity a) = Identity $ f a

---------------------------------------------------------------------------------
--
-- Constant Instance

newtype Constant a b = Constant { getConstant :: a } deriving (Eq, Ord, Show)

instance Functor (Constant a) where
  fmap _ (Constant a) = Constant a

instance Monoid a => Applicative (Constant a) where
  pure x = Constant {getConstant = mempty} 
  (<*>) (Constant e0) (Constant e1) = Constant (e0 <> e1)


---------------------------------------

mPure :: a -> Maybe a
mPure = pure

embed :: Num a => Maybe ((a -> b) -> b)
embed = mPure ($ 2)

mApply :: Maybe ((a -> b) -> b)
      -> Maybe (a -> b)
      -> Maybe b
mApply = (<*>)

myResult = pure ($ 2) `mApply` Just (+2)
-- myResult == Just 4



main = do
  print $ const <$> Just "hello" <*> pure "World"
  print $ (,,,) <$> Just 90 <*> Just 10  <*> Just "Tierness" <*> pure [1, 2, 3]






