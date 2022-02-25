module Main where

import Control.Monad (forever) 
import Data.Char (toLower)
import Data.Maybe (isJust) -- [3]
import Data.List (intersperse) -- [4]
import System.Exit (exitSuccess) -- [5]
import System.Random (randomRIO) -- [6]
import System.IO

type WordList = [String]

allWords :: IO WordList
allWords = do
  dict <- readFile "data/dict.txt"
  return (lines dict)

minWordLength :: Int
minWordLength = 5
maxWordLength :: Int
maxWordLength = 9

gameWords :: IO WordList
gameWords = do
  aw <- allWords
  return (filter gameLength aw)
  where gameLength w =
          let l = length (w :: String)
          in    l >= minWordLength
            &&  l <  maxWordLength

randomWord :: WordList -> IO String
randomWord wl = do
  randomIndex <- randomRIO (0, length wl - 1)
  return $ wl !! randomIndex

randomWord' :: IO String
randomWord' = gameWords >>= randomWord

data Puzzle =
  Puzzle String [Maybe Char] [Char] 

instance Show Puzzle where
   show (Puzzle _ discovered guessed) =
     (intersperse ' ' $
      fmap renderPuzzleChar discovered)
     ++ "\nGuessed so far: " ++ guessed

freshPuzzle :: String -> Puzzle
freshPuzzle theWord =
  Puzzle theWord (replicate sz Nothing) []
    where sz = length theWord

charInWord :: Puzzle -> Char -> Bool
charInWord (Puzzle theWord _ _) x = x `elem` theWord

alreadyGuessed :: Puzzle -> Char -> Bool
alreadyGuessed (Puzzle _ _ xs) x = x `elem` xs

renderPuzzleChar :: Maybe Char -> Char
renderPuzzleChar Nothing    = '_'
renderPuzzleChar (Just x)   = x

fillInCharacter :: Puzzle -> Char -> Puzzle
fillInCharacter (Puzzle word
                  filledInSoFar s) c =
  Puzzle word newFilledInSoFar newGuessed
  where zipper guessed wordChar guessChar = 
          if wordChar == guessed 
            then Just wordChar 
            else guessChar  -- guessChar is a 'Maybe'
        newFilledInSoFar = 
          zipWith (zipper c)
            word filledInSoFar
        newGuessed 
          | c `elem` s  = s
          | otherwise   = c : s

handleGuess :: Puzzle -> Char -> IO Puzzle
handleGuess puzzle guess = do
  putStrLn $ "Your guess was: " ++ [guess]
  case (charInWord puzzle guess, alreadyGuessed puzzle guess) of
    (_, True) -> do
      putStrLn "You already guessed that\
              \ character, pick\
              \ something else!"
    (True, _) -> do
      putStrLn "This character was in the\
              \ word, filling in the word\
              \ accordingly..."
    (False, _) -> do
      putStrLn "This character wasn't in\
              \ the word, try again."
  return (fillInCharacter puzzle guess)

countFailedGuess :: Puzzle -> Int
countFailedGuess (Puzzle wordToGuess _ s) = 
  foldr (\x acc -> if x `elem` wordToGuess then acc else acc + 1) 0 s

gameOver :: Puzzle -> IO ()
gameOver allPuzzle@(Puzzle wordToGuess _ guessed) =
  if (countFailedGuess allPuzzle) > (length wordToGuess) then
    do putStrLn "You lose!"
       putStrLn $ "The word was: " ++ wordToGuess
       exitSuccess
  else return ()

gameWin :: Puzzle -> IO ()
gameWin (Puzzle _ filledInSoFar _) = 
  if all isJust filledInSoFar then
    do putStrLn "You win!"
       exitSuccess
    else return ()

runGame :: Puzzle -> IO ()
runGame puzzle = forever $ do
  gameWin puzzle
  gameOver puzzle
  putStrLn $
    "Current puzzle is: " ++ show puzzle
  putStr "Guess a letter: "
  guess <- getLine
  case guess of
    [c] -> handleGuess puzzle c >>= runGame
    _   -> 
      putStrLn "Your guess must\
              \ be a single character!"

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  word <- randomWord'
  let puzzle = 
        freshPuzzle (fmap toLower word)
  runGame puzzle