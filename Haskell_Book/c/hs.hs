-- import Data.Aeson (encode)
-- import Database.Persist
-- module DetermineTheType where
--  -- simple example
-- example = 1
    {-module Sing where
fstString :: [Char] -> [Char]
fstString x = x ++ " in the rain"

sndString :: [Char] -> [Char]
sndString x = x ++ " over the rainbow"

sing = if (x > y) then fstString x else sndString y 
 where x = "Singin"
       y = "Somewhere"
-}

-- arith3broken.hs

data Identity a = Identity a

instance Eq a => Eq (Identity a) where
    (==) (Identity v) (Identity v') = v == v'

instance Ord a => Ord (Identity a) where
    compare (Identity v)  (Identity v') = compare v  v'

-- 1
data TisAnInteger =
    TisAn Integer

instance Eq TisAnInteger where
    (==) (TisAn v) (TisAn v') = v == v'

--2
data StringOrInt = TisAnInt Int | TisAString String 

instance Eq StringOrInt where
    (==) (TisAnInt v) (TisAnInt v') = v == v'
    (==) (TisAString x) (TisAString x') = x == x'
    (==) _ _ = False

-- 4 

data Pair a = Pair a a

instance Eq a => Eq (Pair a) where
    (==) (Pair v1 v2) (Pair v1' v2') = (v1 == v1') && (v2 == v2')

data Tuple a b = Tuple a b

instance (Eq a, Eq b) => Eq (Tuple a b) where
    (==) (Tuple u1 v1) (Tuple u1' v1') = (u1 == u1') && (v1 == v1')

data Which a = ThisOne a | ThatOne a

instance (Eq a) => Eq (Which a) where
    (==) (ThisOne v) (ThisOne v') = v == v'
    (==) (ThatOne v) (ThatOne v') = v == v'
    (==) _ _ = False

addOneIfOdd = f 
    where f = (\x -> if odd x then x + 1 else x)



main = do
    print $ 7
