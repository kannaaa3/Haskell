import qualified Data.Map as Map
--import Data.Foldable as Foldable

type PhoneNumber    = String
type Name           = String
type PhoneBook      = [(Name, PhoneNumber)]

type AssocList k v  = [(k, v)]

getVal :: (Eq k) => k -> AssocList k v -> Maybe v
getVal key ls = foldr (\x acc ->
                        if (fst x) == key then Just (snd x) else acc
                      ) Nothing ls

type IntMap = Map.Map Int

data Either a b = Left a | Right b deriving (Eq, Ord, Read, Show)

data LockerState = Taken | Free deriving (Show, Eq)

type Code = String
type LockerMap = Map.Map Int (LockerSate, Code)

lockerLookup :: Int -> LockerMap -> Either String Code
lockerLookup lockerNumber map =
    case Map.lookup lockerNumber map of
      Nothing -> Left $ "Locker number " ++ show lockerNumber ++ " doesn't exist!"
      Just (state, code) -> if state /= Taken
                               then Right code
                               else Left $ "Locker " ++ show lockerNumber ++ " is already taken!"


main = do
    let lockers = (Map.fromList [(100,(Taken,"ZD39I")),(101,(Free,"JAH3I")),(103,(Free,"IQSA9")),(105,(Free,"QOTSA")),(109,(Taken,"893JJ")), (110,(Taken,"99292"))]) :: LockerMap
    print $ 3
