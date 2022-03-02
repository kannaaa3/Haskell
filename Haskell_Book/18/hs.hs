import Control.Applicative
import Control.Monad (join, liftM)

andOne :: Num a => a -> [a]
andOne x = [x, 1]

-------------------------------------------------------------------------------------------------

-- concat :: Foldable t => t [a] -> [a]
-- join :: Monad m => m (m a) -> m a

-- liftA :: Applicative f => (a -> b) -> f a -> f b 
-- liftM :: Monad m => (a1 -> r) -> m a1 -> m r
--
bind :: Monad m => (a -> m b) -> m a -> m b 
bind f = join . (fmap f)

bindingAndSequencing :: IO ()
bindingAndSequencing = do
  putStrLn "name pls:"
  name <- getLine
  putStrLn ("y helo thar: " ++ name)

bindingAndSequencing' :: IO ()
bindingAndSequencing' =
  putStrLn "name pls:" >>
  getLine >>=
  \name ->
  putStrLn ("y helo thar: " ++ name)

-------------------------------------------------------------------------------------------------

twoBinds :: IO ()
twoBinds = do
  putStrLn "name pls:"
  name <- getLine
  putStrLn "age pls:"
  age <- getLine
  putStrLn ("y helo thar: "
            ++ name ++ " who is: "
            ++ age ++ " years old.")

twoBinds' :: IO ()
twoBinds' =
  putStrLn "name pls:" >>
  getLine >>=
  \name ->
  putStrLn "age pls:" >>
  getLine >>=
  \age ->
  putStrLn ("y helo thar: "
            ++ name ++ " who is: "
            ++ age ++ " years old.")

-------------------------------------------------------------------------------------------------

-- List Monad
twiceWhenEven'' :: [Integer] -> [Integer]
twiceWhenEven'' xs = 
  xs >>= (\x -> if even x then [x * x, x * x] else [])

twiceWhenEven :: [Integer] -> [Integer]
twiceWhenEven xs = do
  x <- xs
  if even x
    then [x*x, x*x]
    else []

-------------------------------------------------------------------------------------------------

-- Maybe Monad

data Cow = Cow {
    name :: String
  , age :: Int
  , weight :: Int
  } deriving (Eq, Show)

noEmpty :: String -> Maybe String
noEmpty "" = Nothing
noEmpty str = Just str

noNegative :: Int -> Maybe Int
noNegative n  | n >= 0 = Just n
              | otherwise = Nothing

-- if Cow's name is Bess, must be under 500
weightCheck :: Cow -> Maybe Cow
weightCheck c =
  let w = weight c
      n = name c
  in if n == "Bess" && w > 499
      then Nothing
      else Just c

mkSphericalCow :: String
                -> Int
                -> Int
                -> Maybe Cow
mkSphericalCow name' age' weight' = do
  name <- noEmpty name'
  age <- noNegative age' 
  weight <- noNegative weight' 
  weightCheck (Cow name age weight)

mkSphericalCow'' :: String -> Int -> Int -> Maybe Cow
mkSphericalCow'' name' age' weight' =
  noEmpty name' >>=
    \nammy ->
      noNegative age' >>=
        \agey ->
          noNegative weight' >>=
            \weighty ->
            weightCheck (Cow nammy agey weighty)

toMaybe x = Just x
agee :: Int -> Maybe Int
agee x =  noNegative x >>= toMaybe
  
-------------------------------------------------------------------------------------------------

f :: Integer -> Maybe Integer
f 0 = Nothing
f n = Just n

g :: Integer -> Maybe Integer
g i =
  if even i
  then Just (i + 1)
  else Nothing

h :: Integer -> Maybe String
h i = Just ("10191" ++ show i)

doSomething' n = do
  a <- f n
  b <- g a
  c <- h b
  pure (a, b, c)

-- yo ::  Int -> (Int -> Maybe Int) -> Maybe Int 
-- yo x = fmap (return . (*3)) (noNegative x)

main :: IO ()
main = do
  print $ [1..3] >>= return . (+1)  -- fmap (+1) [1..3]
