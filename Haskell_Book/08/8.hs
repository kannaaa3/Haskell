myFactorial :: Int -> Int
myFactorial 0 = 1
myFactorial n = n * myFactorial (n-1)

applyTimes :: (Eq a, Num a) =>
              a -> (b -> b) -> b -> b
applyTimes 0 f b = b
applyTimes n f b = f . applyTimes (n-1) f $ b


f :: Bool -> Maybe Int  
f False = Just 0
f _     = Nothing

dividedBy :: Integral a => a -> a -> (a, a)
dividedBy num denom = go num denom 0
  where go n d count
          | n < d = (count, n)
          | otherwise = go (n - d) d (count + 1)

data DividedResult a = 
      Result a
    | DivideByZero
    deriving (Eq, Show)

dividedByFixed :: Integral a => a -> a -> DividedResult a
dividedByFixed _ 0 = DivideByZero
dividedByFixed num denom = Result (fst . dividedBy num $ denom)

mc91 n 
  | n > 100 = n - 10
  | True    = mc91 . mc91 $ n + 11
