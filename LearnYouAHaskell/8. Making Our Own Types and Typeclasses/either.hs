
data Eithers a b = Lefts a | Rights b deriving (Eq, Ord, Read, Show)

main = do
    let k = Rights 20 
    print $ show k
