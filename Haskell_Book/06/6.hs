import Data.List (sort)
-- import GHC (lieWrappedName)

data Trivial = 
  Trivial

instance Eq Trivial where
  (==) Trivial Trivial  = True

-------------------------------------------------------------------------------------------------

data DayOfWeek = 
  Sun | Mon | Tue | Weds | Thu | Fri | Sat  deriving (Eq, Show)

data Date = 
  Date DayOfWeek Int deriving (Show)

instance Ord DayOfWeek where
  compare Sat Sat = EQ
  compare Sat _   = EQ
  compare _ Sat = LT
  compare _ _     = EQ
------------------------- compare is different from (==)---------------------------------------------       --                  Important:  Ensure it's the same


instance Eq Date where
  (==)  (Date weekday dayOfMonth)
        (Date weekday' dayOfMonth') = 
        weekday == weekday' 
    &&  dayOfMonth == dayOfMonth

-------------------------------------------------------------------------------------------------

newtype Identity a = 
  Identity a

instance Eq a => Eq (Identity a) where
  (==) (Identity v) (Identity v') = v == v'

-------------------------------------------------------------------------------------------------

x :: Rational 
x = toRational 3.14159265358979323846264338327924211

divideThenAdd :: (Fractional a) => a -> a -> a
divideThenAdd x y = (x / y) + 1

add = (+) :: Integer -> Integer -> Integer

-------------------------------------- Enum -----------------------------------------------------------

randomList = enumFromThenTo 1 10 100 -- first, second, max
-- [1,10,19,28,37,46,55,64,73,82,91,100]

-------------------------------------Show------------------------------------------------------------

data Mood = Blah | Woot  deriving (Eq, Show)

  
-------------------------------------------------------------------------------------------------

xxx :: Int -> Int
xxx blah = blah + 20

printIt :: IO ()
printIt = print $ xxx 20 -- putStrLn $ show $ xxx 20 

settleDown x = if x == Woot
      then Blah
      else x

type Subject = String
type Verb = String
type Object = String
data Sentence = Sentence Subject Verb Object
  deriving (Eq, Show)
s1 = Sentence "dogs" "drool"
s2 = Sentence "Julie" "loves" "dogs"

f :: Double
f = 1.0



