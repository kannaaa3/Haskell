import System.IO

type Name = String
type Age = Integer

data Person = Person Name Age deriving Show

data PersonInvalid = NameEmpty | AgeTooLow | PersonInvalidUnknown String deriving (Eq, Show)

mkPerson :: Name -> Age -> Either PersonInvalid Person
mkPerson name age
  | name /= "" && age > 0 = Right $ Person name age
  | name == "" = Left NameEmpty
  | not (age > 0) = Left AgeTooLow
  | otherwise = Left $ PersonInvalidUnknown $ "Name was: " ++ show name ++ " Age was: " ++ show age

gimmePerson :: IO()
gimmePerson = do
  hSetBuffering stdout NoBuffering
  putStr "Enter your name: "
  name <- getLine
  print $ name
  putStr "How old are you? "
  ageString <- getLine 
  let age       = read ageString :: Integer
      thePerson = mkPerson name age
  print $ report thePerson
    where report (Left NameEmpty)       = "This person has an empty name."
          report (Left AgeTooLow)       = "This person's age is too low."
          report (Left _)               = "This person is invalid."
          report (Right (Person name age))= "Yay! Successfully got a person: " ++ name ++ ", " ++ show age
