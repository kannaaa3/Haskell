import Control.Monad
import System.Exit (exitSuccess)
import Data.Char (toLower)
sayHello :: String -> IO ()
sayHello name =
  putStrLn ("Hi " ++ name ++ "!")

palindrome :: IO ()
palindrome = forever $ do
  line1 <- getLine
  let line1' = filter ((flip elem) ['a'..'z']) . fmap toLower $ line1
  case (line1' == reverse line1') of
    True -> putStrLn "It's a palindrome!"
    False -> do 
      putStrLn "Nope!"
      exitSuccess

main  :: IO Bool
main = do
  x1 <- getLine
  x2 <- getLine
  return False
