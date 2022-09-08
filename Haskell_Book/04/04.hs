data Mood = Blah | Woot deriving (Eq, Show)

changeMood :: Mood -> Mood
changeMood Blah = Woot
changeMood _    = Blah

-- if CONDITION then EXPRESSION_A
--              else EXPRESSION_B

-- type alias
--
type Name = String

myName :: Name
myName = "Dung Tran"
