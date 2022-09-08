module Reverse where

import Data.List.Split 

-- dropBefore :: String -> Char -> String  
-- dropBefore s markChar = gen "" s markChar
--    where gen sub "" markChar = reverse sub
--          gen sub (x:xs) markChar 
--             | x == markChar     = gen "" xs markChar
--             | otherwise         = gen (x:sub) xs markChar

rvrs :: String -> String
rvrs x = unwords $ reverse $ splitOn " " x 

main :: IO ()
main = print ()
