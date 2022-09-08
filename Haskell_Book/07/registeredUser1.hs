-- registeredUser1.hs
module RegisterUsered where 

newtype Username = Username String deriving (Eq, Show)
newtype AccountNumber = AccountNumber Integer deriving (Eq, Show)

data User = UnregisteredUser
          | RegisteredUser Username AccountNumber 
          deriving (Eq, Show)


printUser :: User -> IO ()
printUser UnregisteredUser = putStrLn "UnregisteredUser"
printUser (RegisteredUser (Username name)
                          (AccountNumber acctNum))
          = putStrLn $ name ++ ", " ++ show acctNum

    
