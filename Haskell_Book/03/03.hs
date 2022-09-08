module TopOrLocal where

import Reverse

topLevelFunction :: Integer -> Integer
topLevelFunction x =
   x + woot + topLevelValue
   where woot :: Integer
         woot = 10

-- no matter where declare
topLevelValue :: Integer
topLevelValue = 5

area :: Float -> Float
area d = pi  * (r * r)
   where r = d / 2

-------------------------------------------------------------------------------------------------

dropBefore :: String -> Char -> String  
dropBefore s markChar = gen "" s markChar
   where gen sub "" markChar = reverse sub
         gen sub (x:xs) markChar 
            | x == markChar     = gen "" xs markChar
            | otherwise         = gen (x:sub) xs markChar

thirdLetter :: String -> Char
thirdLetter x = x !! 2

letterIndex :: Int -> Char
letterIndex n = curry !! (n-1)
   where curry = "Curry is awesome!"


main :: IO ()
main = do
   putStrLn mySecondString 
   where mySecondString 
            = concat ["hello", " ", "world"]

