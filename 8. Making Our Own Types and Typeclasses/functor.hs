
-- instance Functor [] where
--     fmap = map

data Tree a = EmptyTree | Node a (Tree a) (Tree a) deriving (Show, Read, Eq)

singleton :: a -> Tree a
singleton x = Node x EmptyTree EmptyTree

treeInsert :: (Ord a) => a -> Tree a -> Tree a
treeInsert x EmptyTree = singleton x
treeInsert x (Node a left right)    
  | x == a  = Node a left right
  | x < a   = Node a (treeInsert x left) right
  | x > a   = Node a left (treeInsert x right)

treeElem :: (Ord a) => a -> Tree a -> Bool
treeElem x (Node a left right)
  | x == a  = True
  | x > a   = treeElem x right
  | x < a   = treeElem x left

instance Functor Tree where
    fmap f EmptyTree = EmptyTree
    fmap f (Node x leftsub rightsub) = Node (f x) (fmap f leftsub) (fmap f rightsub)

    {-
        instance Functor (Either a) where
            fmap f (Right b)    = Right (f x)
            fmap f (Left x)     = Left x
        -}

main = do 
    let aTree = foldr treeInsert EmptyTree [5, 4,3,7,1,2,6]
    print $ fmap (^2) aTree
