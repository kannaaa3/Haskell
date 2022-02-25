import System.IO
import Data.Ratio


readInts :: IO [Int]
readInts = fmap (map read.words) getLine

length' :: (Num b) => [a] -> b
length' [] = 0
length' (_ : xs) = 1 + length' xs

sum' :: (Num a) => [a] -> a
sum' [] = 0
sum' (x : xs) = x + sum' xs

tail' :: String -> String
tail' "" = "Empty"
tail' a@(x:"") = a
tail' a@(x:xs) = tail' xs

bmiTell :: (RealFloat a) => a -> a -> String  
bmiTell weight height  
    | bmi <= skinny = "You're underweight, you emo, you!"  
    | bmi <= normal = "You're supposedly normal. Pffft, I bet you're ugly!"  
    | bmi <= fat = "You're fat! Lose some weight, fatty!"  
    | otherwise                 = "You're a whale, congratulations!"  
    where bmi = weight / height ^ 2
          (skinny, normal, fat) = (18.5, 25.0, 30.0)

max' :: (Ord a) => a -> a -> a
max' a b
    | a > b     = a
    | otherwise = b

cmp' :: (Ord a) => a -> a -> Ordering
a `cmp'` b 
    | a > b     = GT
    | a == b    = EQ
    | otherwise = LT

--recur :: (Integral a) => Int -> [a] -> Int
--recur 0 a = head a
--recur z a = do
--    let sum = sum'
--    where sum' = do 
--        sum + recur (z-1) a

--take' :: Int -> [a] -> [a]
--take' _ []       = []
--take' n (x:xs)   = if n <=0 then [] else (x : take' (n-1) xs)


-- Lazy Evaluation
goodSum :: Num a => [a] -> a
goodSum = go 0
    where go acc []         = acc
          go acc (x : xs)   = acc `seq` go (x + acc) xs

sieve :: [Int] -> [Int]
sieve [] = []
sieve (p:xs) = p:sieve[x | x  <- xs, x `mod` p /= 0]

nPrimes n = take n $ sieve [2..]
        
add' x y = x + x 

cal x =
    let a = x + 2
        b = x - 1
    in a * b

ini' :: String -> String -> String
ini' fN lN = [f] ++  "." ++ [l] ++ ""
    where (f : _) = fN
          (l : _) = lN

cylinder :: (RealFloat a) => a -> a -> a
cylinder r h = 
    let sideArea = 2 * pi * r * h
        topArea = pi * r ^ 2
    in sideArea + 2 * topArea


calBmi :: (RealFloat a) => [(a, a)] -> [a]
calBmi xs = [bmi | (w, h) <- xs, let bmi = w / h^2, bmi >= 25.0]

des' :: [a] -> String
des' xs = "Toi la " ++ case xs of 
                         [] ->  "con cho."
                         [x] -> "2 con cho."
                         (x : _) -> "3 con cho."

--des' :: [a] -> String
--des' xs = "Toi la " ++ what xs
--    where 
--        what [] = "con cho."
--        what [x] = "2 con cho."
--        what (x : _) = "3 con cho."

take' :: (Num i, Ord i) => i -> [a] -> [a]
take' n _
    | n <= 0    = [] -- without otherwise part
take' _ []      = []
take' n (x : xs)= x : take' (n - 1) xs 

qS :: (Ord a) => [a] -> [a]
qS [] = []
qS (x : xs) =
    let smallerS    = qS [a | a <- xs, a <= x]
        biggerS     = qS [a | a <- xs, a > x]
    in smallerS ++ [x] ++ biggerS



-- Main 
main = do
    --a' <- getLine
    --let a = read a' :: Float
    --b' <- getLine
    --let b = read b' :: Float
    -- k = [let square x = x * x in (square 5, square 3)]

    let a = [(100, 1), (10, 1)]
        b = [1, 2, 3, 4]
        k = qS "oiyytqraggdhcbn"
        j = (let x = 9 in x ^ 2) + 2

    -- let a = 100; b = 200; c = 300 in a * b * c
    --let (a,b,c) = (1,2,3) in a + b + c

    print k


























