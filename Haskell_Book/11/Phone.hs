import qualified Data.Char as Char
import qualified Data.List as List

data Button = Button 
    {
    repChar     :: Char,
    avaiChar    :: [Char]
    }
    deriving (Show)

data DaPhone = DaPhone [Button]

lstChar :: Int -> [Char]
lstChar 0 = " 0"
lstChar 1 = "1"
lstChar 7 = "PQRS7"
lstChar 8 = "TUV8"
lstChar 9 = "WXYZ9"
lstChar x = (map Char.chr . map (+65) $ [(x-2)*3..((x-1)*3-1)]) ++ show x


myPhone :: DaPhone
myPhone = DaPhone $ 
    [ Button { repChar = x, avaiChar = lstChar $ Char.ord x - 48 } | x <- ['0'..'9'] ]
    ++ [Button{repChar = '#', avaiChar = "#.,"}]
myPhoneButtons :: [Button]
myPhoneButtons = [ Button { repChar = x, avaiChar = lstChar $ Char.ord x - 48 } | x <- ['0'..'9'] ]     ++ [ Button{repChar = '#', avaiChar = "#.,"}]

convo :: [String]
convo =
    ["Wanna play 20 questions",
    "Ya",
    "U 1st haha",
    "Lol ok. Have u ever tasted alcohol",
    "Lol ya",
    "Wow ur cool haha. Ur turn",
    "Ok. Do u think I am pretty Lol",
    "Lol ya",
    "Just making sure rofl ur turn"]

type Digit = Char
-- Valid presses: 1 and up
type Presses = Int

numPress :: Char -> [Char] -> Int
numPress chaar ls = val + 1
    where Just val = List.elemIndex chaar ls

findButton :: Char -> [Button] -> Button
findButton x (thisButton : myBs) 
  | x `elem` (avaiChar thisButton)  = thisButton
  | otherwise                       = findButton x myBs 




reverseTaps :: DaPhone -> Char -> [(Digit, Presses)]
reverseTaps (DaPhone listButton) x =
    let Button {repChar = rc, avaiChar = ac} = findButton (Char.toUpper x) myPhoneButtons
        Just numPress   = List.elemIndex (Char.toUpper x) ac
        ext = if x `elem` ['A'..'Z'] then  [('*', 1)] else []
     in ext ++ [(rc, numPress + 1)]
    
cellPhonesDead :: DaPhone -> String -> [(Digit, Presses)]
cellPhonesDead (DaPhone listButton) theMessage = foldr (\x acc -> (reverseTaps myPhone x) ++ acc) [] (reverse theMessage) 































    {-
data DaPhone = DaPhone [Button]
    deriving (Show)

lstChar :: Int -> [Char]
lstChar 0 = " 0"
lstChar 1 = "1"
lstChar 7 = "PQRS7"
lstChar 8 = "TUV8"
lstChar 9 = "WXYZ9"
lstChar x = (map chr . map (+65) $ [(x-2)*3..((x-1)*3-1)]) ++ show x



normB :: [Button]
normB = [NormButton (lstChar x) | x <- [0..9]]

myPhone :: DaPhone
myPhone = DaPhone $ normB ++ ((SpecButton "#.," ) : Cap : [])

convo :: [String]
convo =
    ["Wanna play 20 questions",
    "Ya",
    "U 1st haha",
    "Lol ok. Have u ever tasted alcohol",
    "Lol ya",
    "Wow ur cool haha. Ur turn",
    "Ok. Do u think I am pretty Lol",
    "Lol ya",
    "Just making sure rofl ur turn"]

type Digit = Char
-- Valid presses: 1 and up
type Presses = Int

numPress :: Char -> [Char] -> Int
numPress chrr ls = val + 1
    where Just val = elemIndex chrr ls

reverseTaps :: DaPhone -> Char -> [(Digit, Presses)]
reverseTaps (DaPhone myP) x
  | x `elem` "#.,"  = [('#',numPress x "#.,")]
  | otherwise       = [((last avai), numPress x avai)]
      where NormButton avai = myP !! (read [x] :: Int)

-- assuming the default phone definition
-- 'a' -> [('2', 1)]
-- 'A' -> [('*', 1), ('2', 1)]
cellPhonesDead :: DaPhone -> String -> [(Digit, Presses)]
cellPhonesDead = undefined










        -}
