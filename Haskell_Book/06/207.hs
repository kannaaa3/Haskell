import Data.List (sort)

newtype Person = Person Bool deriving (Show)

printPerson :: Person -> IO ()
printPerson person = putStrLn (show person)

data Mood = Blah | Woot deriving (Show, Eq)
settleDown x = if x == Woot
                then Blah
                else x

type Subject = String
type Verb = String
type Object = String
data Sentence =
    Sentence Subject Verb Object
    deriving (Eq, Show)
s1 = Sentence "dogs" "drool" "cho"
s2 = Sentence "Julie" "loves" "dogs"
-- hiihi
--
newtype Rocks =
    Rocks String deriving (Eq, Show)
newtype Yeah =
    Yeah Bool deriving (Eq, Show)
data Papu =
    Papu Rocks Yeah
    deriving (Eq, Show)
-- 2, 3
--
--
chk ::  Eq b => (a -> b) -> a -> b -> Bool
chk f x y = f x == y

arith  :: Num b => (a -> b) -> Integer -> a -> b
arith f v x = (^) (f x) v

