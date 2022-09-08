import           Data.List

x = foldr ((++) . show) "" [1..5]

factorial' :: [Integer]
factorial' = scanl (*) 1 [2..]

------------------------------------- Chapter Exercises ------------------------------------------------------------

stops = "pbtdkg"
vowels = "aeiou"

svs = [[x, y, z] | x <- stops, y <- vowels, z <- stops ]
svsp = [['p', y, z] | y <- vowels, z <- stops ]

myAny :: (a -> Bool) -> [a] -> Bool
myAny f = foldr ((&&) . f) True

myMaximumBy :: Num a => (a -> a -> Ordering)
            -> [a]
            -> a
myMaximumBy f = foldr selectF 0
  where selectF a b
          |  f a b == GT       = a
          | otherwise          = b

