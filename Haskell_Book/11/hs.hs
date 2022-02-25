{-# LANGUAGE GeneralizedNewtypeDeriving #-} -- pragma

class TooMany a where
    tooMany :: a -> Bool
instance TooMany Int where
    tooMany n = n > 42
newtype Goats =
    Goats Int deriving (Eq, Show, TooMany) -- no need to write instance

    {-
data FlowerType = Gardenia
                | Daisy
                | Rose
                | Lilac
                deriving Show

        -}

type Gardener = String

data Garden = Gardenia Gardener | Daisy Gardener | Rose Gardener
    deriving Show

-- Constructing and deconstructing values
--

data GuessWhat = Chickenbutt deriving (Eq, Show) -- trivial ()
data Id a = MkId a deriving (Eq, Show)
data Product a b = Product a b deriving (Eq, Show)
data Sum a b = First a | Second b deriving (Eq, Show)
data RecordProduct a b = RecordProduct { pfirst :: a , psecond :: b } deriving (Eq, Show)

myRecord :: RecordProduct Integer Float
myRecord = RecordProduct { 
    pfirst = 42, 
    psecond = 0.00001 }

-- pfirst myRecord

data OperatingSystem = GnuPlusLinux | OpenBSDPlusNevermindJustBSDStill | Mac | Windows deriving (Eq, Show)
data ProgLang = Haskell | Agda | Idris | PureScript deriving (Eq, Show)
data Programmer = Programmer { os :: OperatingSystem , lang :: ProgLang } deriving (Eq, Show)



allOperatingSystems :: [OperatingSystem]
allOperatingSystems = [ GnuPlusLinux , OpenBSDPlusNevermindJustBSDStill , Mac , Windows ]

allLanguages :: [ProgLang]
allLanguages = [Haskell, Agda, Idris, PureScript]

allProgrammers :: [Programmer]
allProgrammers = [ Programmer { os = x, lang = y} | x <- allOperatingSystems, y <- allLanguages ]

-- Works the same as if
-- we'd used record syntax.
data ThereYet = There Float Int Bool deriving (Eq, Show)
-- who needs a "builder pattern"?
notYet :: Int -> Bool -> ThereYet
notYet = There 25.5

notQuite :: Bool -> ThereYet
notQuite = notYet 10

yusssss :: ThereYet
yusssss = notQuite False

-- Split out the record/product
data Car = Car { make :: String , model :: String , year :: Integer } deriving (Eq, Show)
-- The Null is still not great, but
-- we're leaving it in to make a point
data Automobile = Null | Automobile Car deriving (Eq, Show)

foldTree :: (a -> b -> b) -> b -> BinaryTree a -> b
foldTree f acc Leaf   = acc
foldTree f acc (lft x rgt)      = foldTree f acc'' lft
    where acc' = foldTree f acc rgt
          acc''= f x rgt
