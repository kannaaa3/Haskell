x = 0

avgGrade :: (Fractional a, Ord a) 
         => a -> Char
avgGrade x
  | y >= 0.9    = 'A'
  | y >= 0.5    = 'F'
  | y >= 0.8    = 'B'
  | True        = 'C'
    where y = x / 100

-- print = putStrLn , show

tensDigit :: Integral a => a -> a
tensDigit x = d
    where xLast = x `div` 10
          d = xLast `mod` 10
