module DatabaseProccess where 

import Data.Time
import Data.List

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
  , DbNumber 9010
  ]

filterDbDate :: [DatabaseItem]
             -> [UTCTime]
filterDbDate = foldr f []
  where f (DbDate theUTCTime) xs = theUTCTime : xs
        f _                   xs = xs

filterDbNumber :: [DatabaseItem]
               -> [Integer]
filterDbNumber = foldr f []
  where f (DbNumber theInteger) xs = theInteger : xs
        f _                     xs = xs

mostRecent :: [DatabaseItem]
           -> UTCTime
mostRecent = maximum . filterDbDate

sumDb :: [DatabaseItem]
      -> Integer
sumDb = sum . filterDbNumber

avgDb :: [DatabaseItem]
      -> Double
avgDb xs = (/ szDbNumber) . fromInteger . sumDb $ xs
  where szDbNumber =  genericLength $ filterDbNumber xs
