{-# LANGUAGE FlexibleInstances  #-}


import Data.Monoid
import Control.Applicative

-- fmap f x = pure f <*> x

data Flip f a b = 
  Flip (f b a)
  deriving (Eq, Show)

newtype Aithe a b= Aithe (Either a b)

instance Functor (Flip Aithe a) where
  fmap f (Flip (Aithe (Left a)))  = Flip $ Aithe $ Left (f a)
  fmap _ (Flip (Aithe (Right b))) = Flip $ Aithe $ Right b

instance Functor (Flip Either a) where
  fmap f (Flip (Left a))  = Flip $ Left (f a)
  fmap _ (Flip (Right b)) = Flip $ Right b

-- instance Applicative (Flip Either a) where
--   (<*>) ff fa = fmap 


main :: IO ()
main = do
  -- print $ ("Woo", (+1)) <*> (" Hoo!", 0)
  -- print $ (Product 3, (+9)) <*> (Product 2, 8)
  -- print $ [(,)] <*> [1,2] <*> [3,4] -- liftA2 (,) [1,2] [3,4]
  x <- ((++) <$> getLine <*> getLine)
  print $ x
  return ()
