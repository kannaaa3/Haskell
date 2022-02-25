import qualified Data.List as List
ok :: String -> Bool
ok xs = foldr (\x acc -> if x `notElem` xs then False else acc) True "123456789"


--39 186 7254
f1 :: Int -> Int -> [Int] -> [Int]
f1 i j acc
  | i > 99                                  = acc
  | i * j > 9999                            = f1 (i + 1) (1000 `div` i) acc
  | ok $ show i ++ show j ++ show (i * j)   = (i * j) : (f1 i (j + 1) acc)
  | otherwise                               = f1 i (j + 1) acc 

main = do 
    print $ sum . List.nub  $ f1 1 1000 []
