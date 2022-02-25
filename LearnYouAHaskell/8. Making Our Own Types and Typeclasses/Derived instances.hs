data Person = Person
    {
    firstName   :: String,
    lastName    :: String,
    age         :: Int
    } deriving (Eq, Show, Read)

data Bul =  False | True deriving (Ord, Eq) -- defined first -> smaller
data Day =  Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday  -- nullary : take no parameters, i.e. firleds
    deriving (Eq, Ord, Show, Read, Bounded, Enum)



main = do 
    let mikeD   = Person {firstName = "Michael", lastName = "Diamond", age = 43}
        adRock  = Person {firstName = "Adam", lastName = "Horovitz", age = 41}
        mca = Person {firstName = "Adam", lastName = "Yauch", age = 44}
    let cho = [mca, adRock, mikeD]
    print $ mikeD == adRock
    print $ mikeD `elem` cho
    print $ show mikeD
    let c = read "Person {firstName =\"Michael\", lastName =\"Diamond\", age = 43}" :: Person  -- == mikeD
    print $ firstName c
    print $ Just 3 `compare` Just 2
    print $ Nothing > Just (-4999999)
    -- read "Just 't'" :: Maybe a !!!!! -> Maybe Char
    print $ (minBound :: Day) 
    print $ succ Monday -- pred Saturday
    print $ ([minBound .. maxBound] :: [Day])

