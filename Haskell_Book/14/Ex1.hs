module WordNumberTest where

import Test.Hspec
import Test.QuickCheck
import WordNumber
  (digitToWord)
import Data.List (sort)

half :: (Fractional a) => a -> a
half x = x / 2
halfIdentity = (*2) . half

-- for any list you apply sort to
-- this property should hold
listOrdered :: (Ord a) => [a] -> Bool
listOrdered xs =
  snd $ foldr go (Nothing, True) xs
  where go _ status@(_, False) = status
        go y (Nothing, t) = (Just y, t)
        go y (Just x, t) = (Just y, x >= y)

plusAssociative x y z =
  x + (y + z) == (x + y) + z
plusCommutative x y =
  x + y == y + x

-- quot rem
checkQuot x y 
  | y == 0  = True
  | True    = (quot x y) * y + (rem x y) == x
checkDiv x y = (div x y)  * y + (mod x y) == x

roundTrip x = (read (show x)) == x





twice f = f . f
fourTimes = twice . twice


f' x =
  (sort x
  == twice sort x)
  &&
  (sort x
  == fourTimes sort x)

data Sum a b = First a | Second b 
  deriving (Eq, Show)

sumGenEqual :: (Arbitrary a, Arbitrary b) => Gen (Sum a b)
sumGenEqual = do 
  a <- arbitrary
  b <- arbitrary
  oneof [return $ First a,
         return $ Second b]

sumGenCharInt :: Gen (Sum Char Int)
sumGenCharInt = sumGenEqual

----- My Maybe
data Maibe a = Nothimg | Jut a deriving (Eq, Show)

-- instance Arbitrary a =>
--          Arbitrary (Maibe a) where
--   arbitrary =
--     frequency [(1, return Nothimg),
--                 (3, liftM Jut arbitrary)]


sumGenFirstPls :: (Arbitrary a, Arbitrary b) => Gen (Sum a b)
sumGenFirstPls = do 
  a <- arbitrary
  b <- arbitrary
  frequency [(4, return $ First a),
              (1, return $ Second b)]

sumGenFirstPlsIntChar :: Gen (Sum Int Char)
sumGenFirstPlsIntChar = sumGenFirstPls



-- CoArbitrary: Enables the generation of FUNCTION fitting a type
-- deriving Generic => get instance 4free
--
--

---------------------------------------

data Fool = 
    Fulse
  | Frue
  deriving (Eq, Show)

genEqualFool :: Gen Fool
genEqualFool = 
  oneof [return $ Fulse, return $ Frue]

genFulsePls :: Gen Fool
genFulsePls = 
  frequency [(2, return $ Fulse),
             (1, return $ Frue)]

---------------------------------------



---------------------------------------

---------------------------------------

---------------------------------------

---------------------------------------

---------------------------------------

---------------------------------------

---------------------------------------


main :: IO ()
main = hspec $ do
 describe "AAAAAA" $ do
   it "returns zero for 0" $ do
     digitToWord 0 `shouldBe` "zero"
   it "returns one for 1" $ do
     digitToWord 1 `shouldBe` "cho"
    

   describe "Half check" $ do
   it "who knoes" $ do
     property $ \x -> (x :: Double) == halfIdentity x
   it "sort things" $ do
     property $ \x -> listOrdered $ sort (x :: [Int])
   it "plus thingy" $ do
     property $ \x y z -> plusAssociative x y (z :: Int)
       -- it "div quot" $ do
       --   property $ \x y -> checkQuot x (y :: Int)
       -- it "round trip" $ do
       --   property $ \x -> roundTrip (x :: Int)
       -- it "idempotence" $ do
       --   property $ \x -> f' (x :: [Int])

  --574
  {-
  describe "digits" $ do
    it "returns [1] for 1" $ do
      digits 1 `shouldBe` [1]
    it "returns [1, 0, 0] for 100" $ do
      print "???"

  describe "wordNumber" $ do
    it "one-zero-zero given 100" $ do
      wordNumber 100
      `shouldBe` "one-zero-zero"
    it "nine-zero-zero-one for 9001" $ do
      print "???"
   
   - -}
