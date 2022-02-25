import Data.Time

data DatabaseItem = DbString String
                    | DbNumber Integer
                    | DbDate UTCTime
                    deriving (Eq, Ord, Show)

theDatabase :: [DatabaseItem]
theDatabase =
    [ DbDate (UTCTime
        (fromGregorian 1911 5 1)
        (secondsToDiffTime 34123))
    , DbNumber 9001
    , DbString "Hello, world!"
    , DbDate (UTCTime
        (fromGregorian 1921 5 1)
        (secondsToDiffTime 34123))
    ]

funcho :: DatabaseItem -> [UTCTime] -> [UTCTime]
funcho (DbDate t) xs = t : xs 
funcho _ xs = xs

filterDbDate :: [DatabaseItem] -> [UTCTime]
filterDbDate = foldr funcho [] 

mostRecent :: [DatabaseItem] -> UTCTime
mostRecent = foldr max minUTC . filterDbDate
    where minUTC = UTCTime (fromGregorian 0 0 0) (secondsToDiffTime 0)

main = do
    print 0
