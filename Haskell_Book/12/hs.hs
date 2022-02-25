import Data.List as List

notThe :: String -> String
notThe "The"    = "A"
notThe "the"    = "a"
notThe x        = x

replaceThe :: String -> String
replaceThe prevString = 
    let listWords   = fmap notThe . words $ prevString 
    in    concat . intersperse " " $ listWords

countVowels :: String -> Int
countVowels = length . filter (flip elem "aeuoi")


newtype Word' = Word' String 
    deriving (Eq, Show)

vowels = "aeiou"

mkWord :: String -> Maybe Word'
mkWord s 
    | validateWord s    = Just $ Word' s
    | otherwise         = Nothing
    where validateWord aWord =
            let numVowels       = countVowels s
                numConsonants   = length s - numVowels
            in numVowels < numConsonants

data Nat = Zero
         | Succ Nat
         deriving (Eq, Show)

natToInteger :: Nat -> Integer
natToInteger Zero       = 0
natToInteger (Succ x)   = 1 + natToInteger x

integerToNat :: Integer -> Maybe Nat
integerToNat x
  | x < 0   = Nothing
  | x == 0  = Just Zero
  | True    = Just (Succ val)
    where Just val = integerToNat $ x - 1


--- 1
isJust :: Maybe a -> Bool
isJust Nothing  = False
isJust _        = True

isNothing :: Maybe a -> Bool
isNothing Nothing   = True
isNothing _         = False

--- 2

mayybee :: b -> (a -> b) -> Maybe a -> b
mayybee fallbackVal _ Nothing   = fallbackVal
mayybee _ f (Just val)         = f val

listToMaybe :: [a] -> Maybe a
listToMaybe []          = Nothing
listToMaybe (x : xs)    = Just x

maybeToList :: Maybe a -> [a]
maybeToList Nothing = []
maybeToList (Just x)  = [x]

catMaybes :: (Eq a) => [Maybe a] -> [a]
catMaybes maybeList = [x | Just x <- justList]
    where justList = (filter (/= Nothing) maybeList)

flipMaybe :: (Eq a) => [Maybe a] -> Maybe [a]
flipMaybe aMaybeList
  | Nothing `elem` aMaybeList    = Nothing
  | otherwise                   = Just $ catMaybes aMaybeList

leftToMaybe  :: Either a b -> Maybe a
leftToMaybe  (Left x)    = Just x
leftToMaybe   _           = Nothing

rightToMaybe :: Either a b -> Maybe b
rightToMaybe (Right x)    = Just x
rightToMaybe _           = Nothing

lefts' :: (Eq a) => [Either a b] -> [a]
lefts' x = catMaybes $ foldr (\x acc -> leftToMaybe x : acc) [] x

rights' :: (Eq b) => [Either a b] -> [b]
rights' x = catMaybes $ foldr (\x acc -> rightToMaybe x : acc) [] x

partitionEithers' :: (Eq a, Eq b) => [Either a b] -> ([a], [b])
partitionEithers' eitherList = (lefts' eitherList, rights' eitherList)

------------
eitherMaybe' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe' f (Right val)  = Just (f val)
eitherMaybe' _ _            = Nothing

either' :: (a -> c) -> (b -> c) -> Either a b -> c
either' f _ (Left a)    = f a
either' _ f (Right b)   = f b


---- Whole 'nother level
--




main = do
    print $ fmap Just [1,2,3]
    print $ take 10 $ iterate (+1) 0
