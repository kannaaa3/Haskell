data Person = Person
    {
    firstname   :: String,
    lastName    :: String,
    age         :: Int,
    height      :: Float,
    phoneNummer :: String,
    flavor      :: String
    } deriving (Show) -- record syntax

data Car = Car
    {
    company     :: String,
    model       :: String,
    year        :: Int
    } deriving (Show)

main = do
    let guy     = Person "Buddy" "Finklestein" 43 184.2 "526-2928" "Chocolate"  
        acar    = Car "Ford" "Mustang" 1967
    print $ flavor guy
    print $ guy
    print $ model acar
