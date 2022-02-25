-- class Eq a where
--     (==) :: a -> a -> Bool
--     (/=) :: a -> a -> Bool
--     x == y = not (x /= y)
--     x /= y = not (x == y)

data TrafficLight = Red | Yellow | Green

instance Eq TrafficLight where
    Red == Red = True
    Green == Green = True
    Yellow == Yellow = True
    _ == _ = False

instance Show TrafficLight where
    show Red = "Red light"
    show Green = "Green Lait"
    show Yellow = "Da low light"

-- Class constraint
{-
instance (Eq m) => Eq (Maybe m) where
    Maybe x == Maybe y = x == y
    Nothing == Nothing = True
    _ == _ = False
-}

-- A yes-no typeclass

class YesNo a where
    yesno :: a -> Bool

instance YesNo Int where
    yesno 0 = False
    yesno _ = True

instance YesNo [a] where
    yesno []    = False
    yesno _     = True

instance YesNo Bool where
    yesno = id

instance YesNo (Maybe a) where
    yesno Nothing   = False
    yesno _         = True

-- instance YesNo (Tree a) where
--     yesno EmptyTree = False
--     yesno _         = True

instance YesNo TrafficLight where
    yesno Red   = False
    yesno _     = True

yesnoIf :: (YesNo y) => y -> a -> a -> a
yesnoIf yesnoVal yesResult noResult = if yesno yesnoVal then yesResult else noResult

main = do
    print $ yesno Red
    print $ yesnoIf [] "cho" "kocho"

