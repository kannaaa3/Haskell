{-# LANGUAGE NoMonomorphismRestriction #-}

import Data.Bifunctor as Bifu


mTh :: Integer -> Integer -> Integer -> Integer
mTh = \x y z -> x * y * z

adOneIfOdd n = if odd n then f n else n
  where f = (+1)

addFive :: (Num a, Ord a) => a -> a -> a
addFive = \x y -> (if x > y then x else y) + 5

mflip f x y = f y x

---------------------------------- Cases---------------------------------------------------------------

funcY xs = 
  case xs == reverse xs of
    True  ->  "yes"
    False ->  "no" 

nums x = 
  case compare x 0 of
    LT -> -1
    GT -> 1
    EQ -> 0

----------------------------------- Artful Dodgy--------------------------------------------------------------

dodgy x y = x + y * 10
oneIsOne  = dodgy 1
oneIsTwo = (flip dodgy) 2 -- without pragma, would be Integer


------------------------------------ Guards -------------------------------------------------------------


bloodNa :: Integer -> String
bloodNa x
  | x < 135= "too low"
  | x > 145= "too high"
  | otherwise = "just right"

avgGrade :: (Fractional a, Ord a)
          => a -> Char
avgGrade x
    | y >= 0.9  = 'A'
    | y >= 0.8  = 'B'
    | y >= 0.7  = 'C'
    | y >= 0.59 = 'D'
    | otherwise  = 'F' -- dude why without this, it isn't matched (y < 0.59)
    where y = x / 100

------------------------------ composition -------------------------------------------------------------------
f :: [Char] -> Int
f = length . filter (== 'a')
-- f "abrajkajkaha"
-- 5

-- print = putStrLn . show

g :: (a -> b) -> (a, c) -> (b, c)
g = Bifu.first
-- g f tp = (f . fst $ tp, snd tp)

------------------------------------ What -------------------------------------------------------------

-- not pointfree
-- blah x = x
-- addAndDrop x y = x + 1
-- reverseMkTuple a b = (b, a)
-- reverseTuple (a, b) = (b, a)
-- pointfree versions of the above

blah = id
addAndDrop = const . (1 +)
reverseMkTuple = flip (,)
reverseTuple = uncurry (flip (,))


