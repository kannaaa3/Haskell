import System.IO

isU :: Char -> Bool
isU = (`elem` ['A'..'Z'])

applyTwice :: (a -> a) -> a -> a
applyTwice f x = f (f x)

zW :: (a -> b -> c) -> [a] -> [b] -> [c]
zW _ [] _ = []
zW _ _ [] = []
zW f (x : xs) (y : ys) = f x y : zW f xs ys

flip' :: (a -> b -> c) -> b -> a -> c
flip' f x y = f y x
--    let f y x = g x y
--     in g

f :: (Num a) => a -> a -> a
f x y = x - y

mm2 :: (Num a) => [[a]] -> [[a]]
mm2 = map f
    where f = map (^2)

f' :: (a -> Bool) -> [a] -> [a]
f' _ [] = []
f' check (x :  xs)
  | check x   = x : f' check xs
  | otherwise = f' check xs

qS :: (Ord a) => [a] -> [a]
qS [] = []
qS (x : xs) = 
    let lefts   = qS (f' (<= x) xs) 
        rights  = qS (f' (> x) xs )
    in lefts ++ [x] ++ rights

add' :: (Num a) => a -> a -> a
add' x y = x + 2 * y

add'' :: (Num a) => a -> a
add'' = add' 9

chain :: Int -> [Int]
chain 1 = [1]
chain n 
    | odd n     = n : chain (n * 3 + 1)
    | even n    = n : chain (div n 2)

--numLongChain :: Int
--numLongChain = length (filter isLong (map chain [1..100]))
--    where isLong xs = length xs > 15

--numLongChain :: Int 
--numLongChain = length (filter (\xs -> length > 15) (map chain [1..100])) 

sSum :: (Num a) => (a -> a -> a) -> a -> [a] -> a
sSum _ acc [] = acc
sSum f acc (x : xs) = sSum f (f acc x) xs
    --where go _ acc [] = acc
    --      go f acc (x : xs) = go (f acc x) xs


sum0' :: (Num a) => [a] -> a
sum0' = foldl (+) 0 

sep :: (String, String) -> [String]
sep (a, []) = [a]
sep ([], (x : rmS)) = sep([x], rmS)
sep (s, (x : rmS)) 
    | (s `elem` [[x] | x <- "+-*/()"])  = s : (sep ([x], rmS))
    | x `elem` "+-*/()"                 = s  : sep ([x], rmS)
    |  otherwise                        = sep (s ++ [x], rmS)

elem' :: (Eq a) => a -> [a] -> Bool  
elem' y ys = foldl (\acc x -> if x == y then True else acc) False ys

mm3' :: (Num a) => (a -> a) -> [a] -> [a]
mm3' f xs = foldr (\x acc -> f x : acc) [] xs

revs :: [a] -> [a]
revs xs = foldl (\acc x -> x : acc) [] xs

ggcd :: Int -> Int -> Int 
ggcd a b 
    | a > b = ggcd b a
    | a == 0 = b
    | otherwise = ggcd (b `mod` a) a

-- packk :: String -> [(Int, Char)]
-- packk = foldr func []
--     where   func curChar cnt [] = [(cnt, curChar)]
--             func curChar cnt (y : xs) =
--                 if curChar == head y 
--                    then (())




main = do
    -- flip' zip [1..5] "hello" 
    -- [h 1 ...]
--    let xs = [1..]
--    let k = sum (takeWhile (< 100) [x^2 | x <- xs, odd x])
    --let lst = map add' [3,4,9]
    --    k   = (lst !! 2) 7
    -- Lamdas Function
    print $ map (negate . abs) [1, -3, 7]
    print $ map ($ 3) [(4 +), (* 10), (^2)]













