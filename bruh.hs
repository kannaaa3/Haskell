data Tree a = Leaf | Bin a (Tree a) (Tree a)
    deriving (Eq, Show)

tinc :: Tree Int    -> Tree Int
tinc Leaf           = Leaf 
tinc (Bin i l r)    = Bin (i + 1) (tinc l) (tinc r)

tmap :: (a -> b) -> Tree a -> Tree b
tmap f Leaf         = Leaf
tmap f (Bin i l r)  = Bin (f i) (tmap f l) (tmap f r)

main = do 
    let k = 2
    print k
