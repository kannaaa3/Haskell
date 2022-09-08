
module PoemLines where


enumFromTo' :: (Enum a, Ord a) => a -> a -> [a]
enumFromTo' = go
  where go st ed
          | st > ed   = [] 
          | otherwise = st : go (succ st) ed



enum' :: (Enum a, Eq a) => a -> a -> [a]
enum' = go
    where go st ed 
            | st == ed  = [ed]
            | otherwise = st : (go (succ st) ed)

spl :: String -> [String]
spl []  = []
spl str 
  | null $ dropWhile (/= ' ') str   = [str]
  | otherwise                       = fstWord : (spl remstr)
    where fstWord       = takeWhile (/= ' ') str
          (x : remstr)  = dropWhile (/= ' ') str

firstSen = "Tyger Tyger, burning bright\n"
secondSen = "In the forests of the night\n"
thirdSen = "What immortal hand or eye\n"
fourthSen = "Could frame thy fearful symmetry?"
sentences = firstSen ++ secondSen
        ++ thirdSen ++ fourthSen

x = [1] ++ undefined ++ [3]

mylength :: [a] -> Integer
mylength [] = 0
mylength (x : xs) = 1 + mylength xs

k = take 1 $ filter even [1,2,3,undefined]
