{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

newtype DogueDeBordeaux doge =
  DogueDeBordeau doge

newtype Price =
    Price Integer deriving (Eq, Show)

data Manufacturer = --
                  Mini --
                  | Mazda --
                  | Tata --
                  deriving (Eq, Show)

data Airline = --
              PapuAir --
              | CatapultsR'Us --
              | TakeYourChancesUnited --
              deriving (Eq, Show)

data Vehicle = Car Manufacturer Price
              | Plane Airline
              deriving (Eq, Show)

--------------------------------------- newtype ----------------------------------------------------------
newtype Goats =
  Goats Int deriving (Eq, Show, TooMany)   -- pragma helps us
newtype Cows =
  Cows Int deriving (Eq, Show)

class TooMany a where
  tooMany :: a -> Bool

instance TooMany Int where
  tooMany n = n > 42

instance TooMany (Int, String) where -- require FlexibleInstances
  tooMany (x, _) = tooMany x

instance (Num a, TooMany a) => TooMany (a, a) where
  tooMany aa = or . fmap tooMany $ [fst aa, snd aa]


----------------------------------------- os --------------------------------------------------------
data OperatingSystem =
            GnuPlusLinux
          | OpenBSDPlusNevermindJustBSDStill
          | Mac
          | Windows
          deriving (Eq, Show)
data ProgLang =
            Haskell
          | Agda
          | Idris
          | PureScript
          deriving (Eq, Show)
data Programmer =
  Programmer { os    :: OperatingSystem
              , lang :: ProgLang }
    deriving (Eq, Show)

allOperatingSystems :: [OperatingSystem]
allOperatingSystems =
  [ GnuPlusLinux
  , OpenBSDPlusNevermindJustBSDStill
  , Mac
  , Windows
  ]

allLanguages :: [ProgLang]
allLanguages =
  [Haskell, Agda, Idris, PureScript]

allProgrammers :: [Programmer]
allProgrammers = [Programmer x y | x <- allOperatingSystems, y <- allLanguages]

-------------------------------------- percolate, not bottom-----------------------------------------------------------

-- Works the same as if
-- we'd used record syntax.
data ThereYet =
  There Float Int Bool
  deriving (Eq, Show)
-- who needs a "builder pattern"?
notYet :: Int -> Bool -> ThereYet
notYet = There 25.5

notQuite :: Bool -> ThereYet
notQuite = notYet 10

yusssss :: ThereYet
yusssss = notQuite False

-------------------------------------------- Deconstructing Value-----------------------------------------------------
-- Just pattern matching and breaking down the structure
newtype Name= Name String deriving Show
newtype Acres= Acres Int deriving Show

data FarmerType = DairyFarmer
                | WheatFarmer
                | SoybeanFarmer
                deriving Show
-- Farmer is a plain ole product of
data FarmerRec =
  FarmerRec { name       :: Name
            , acres      :: Acres
            , farmerType :: FarmerType }
            deriving Show

isDairyFarmerRec :: FarmerRec -> Bool
isDairyFarmerRec farmer =
  case farmerType farmer of
  DairyFarmer -> True
  _           -> False

----------------------------------- List --------------------------------------------------------------
data List a = Nil
            | Cons a (List a)
            deriving (Eq, Show)

------------------------------------Binary Tree--------------------------------------------------------
data BinaryTree a =
    Leaf
  | Node (BinaryTree a) a (BinaryTree a)
  deriving (Eq, Show, Ord)

insert' :: Ord a 
        => a 
        -> BinaryTree a
        -> BinaryTree a
insert' val Leaf  = Node Leaf val Leaf
insert' val (Node left a right) 
  | val == a  = Node left a right
  | val < a   = Node (insert' val left) a right
  | otherwise = Node left a (insert' val right)


mapTree :: (a -> b)
        -> BinaryTree a
        -> BinaryTree b
mapTree _ Leaf = Leaf
mapTree f (Node left a right) =
  Node (mapTree f left) (f a) (mapTree f right)

testTree' :: BinaryTree Integer
testTree' = Node (Node Leaf 3 Leaf) 1 (Node Leaf 4 Leaf)
mapExpected = Node (Node Leaf 4 Leaf) 2 (Node Leaf 5 Leaf)
-- acceptance test for mapTree
mapOkay = 
    if mapTree (+1) testTree' == mapExpected
    then print "yup okay!"
    else error "test failed!"


preorder :: BinaryTree a -> [a]
preorder = undefined

inorder :: BinaryTree a -> [a]
inorder = undefined

postorder :: BinaryTree a -> [a]
postorder = undefined
testTree :: BinaryTree Integer
testTree =
  Node (Node Leaf 1 Leaf) 2 (Node Leaf 3 Leaf)

testPreorder :: IO ()
testPreorder =
  if preorder testTree == [2, 1, 3]
  then putStrLn "Preorder fine!"
  else putStrLn "Bad news bears."

testInorder :: IO ()
testInorder =
  if inorder testTree == [1, 2, 3]
  then putStrLn "Inorder fine!"
  else putStrLn "Bad news bears."
testPostorder :: IO ()
testPostorder =
  if postorder testTree == [1, 3, 2]
  then putStrLn "Postorder fine!"
  else putStrLn "postorder failed check"

-- foldTree :: (a -> b -> b)
--          -> b
--          -> BinaryTree a
--          -> b
-- foldTree  

main :: IO ()
main = do
  testPreorder
  testInorder
  testPostorder
