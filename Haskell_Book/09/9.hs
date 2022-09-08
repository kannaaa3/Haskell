-- import Data.List as L

myWords :: String -> [String]
myWords "" = []
myWords xs = 
    let curStr = takeWhile (/= ' ') xs
        remStr = dropWhile (/= ' ') xs
    in curStr : myWords (dropWhile (== ' ') remStr)

myLilList :: [Int]
myLilList = [x^2 | x <- [1..10]]
-- myLilList = [1,4,9,...,100]

------------------------------- String ------------------------------------------------------------------

myFilter :: String -> [String]
myFilter = filter (`notElem` ["a", "an", "the"]) . words

-------------------------------- Exercises -----------------------------------------------------------------
squish :: [[a]] -> [a]
squish = foldr (++) [] -- concat
