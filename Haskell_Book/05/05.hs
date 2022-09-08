nonsense :: Bool -> Integer
nonsense True = 805
nonsense False = 31337


---------------------------- Same Function---------------------------------------------------------------------

anonymous :: Integer -> Bool -> Integer
anonymous = \i b -> i + nonsense b

anonNested :: Integer
            -> Bool
            -> Integer
anonNested = 
  \i -> \b -> i + nonsense b

-------------------------------------------------------------------------------------------------

curry :: ((a, b) -> c) -> a -> b -> c
curry f a b = f (a, b)

uncurry' :: (a -> b -> c) -> (a, b) -> c
uncurry' f (a, b) = f a b

-------------------------------------------------------------------------------------------------
-- fromIntegral: Integral -> Num
xInt = 20 :: Integer

xNum :: Num a => a
xNum = fromIntegral xInt

main :: IO ()
main = do
  print $ uncurry' (+) (1, 2)
  print $ 9 `elem` [1..10]



