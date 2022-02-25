module Ciphers where

-- chr and ord 

squish :: (Eq a) => [[a]] -> [a]
squish [] = []
squish ((y:ys) : xs) 
  | ys == [] = y : squish xs 
  | True    = y : (squish (ys : xs))
