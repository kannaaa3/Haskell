-- registeredUser1.hs
    {-
        module RegisteredUser where

        newtype Username = Username String
        newtype AccountNumber = AccountNumber Integer

        data User = UnregisteredUser
                  | RegisteredUser Username AccountNumber 

        printUser :: User -> IO ()
        printUser UnregisteredUser = putStrLn "UnregisteredUser"
        printUser (RegisteredUser (Username name) (AccountNumber acctNum))
            = putStrLn $ name ++ " " ++ show acctNum

        -}
-- matchingTuples1.hs
module Cho where
k (x, y) = x
k1 = k ((4-1), 10)
k2 = k ("three", (1 + 2))
k3 = k (3, True)

funcZ x = 
    case cho of
      True -> "AWESOME"
      False -> "wut"
    where cho = x + 1 == 1

functionC x y =
    case x > y of
      True -> x
      False -> y

ifEvenAdd2 n =
    case even n of
      True -> n + 2
      False -> n

nums x =
    case compare x 0 of
      LT -> -1
      GT -> 1
      EQ -> 0


myFlip :: (a -> b -> c) -> b -> a -> c
myFlip f = \ x y -> f y x


