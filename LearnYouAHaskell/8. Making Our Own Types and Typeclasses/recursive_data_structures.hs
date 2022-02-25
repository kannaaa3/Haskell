infixr 6 :-:
data List a = Empty | a :-: (List a) deriving (Show, Read, Eq, Ord)

infixr 5 .++
(.++) :: List a -> List a -> List a
Empty .++ ys = ys
(x :-: xs) .++ ys = x :-: (xs .++ ys)


data Tree a = EmptyTree | Node a (Tree a) (Tree a) deriving (Show, Read, Eq)

singleton :: a -> Tree a
singleton a = Node a EmptyTree EmptyTree

treeInsert :: (Ord a) => a -> Tree a -> Tree a
treeInsert x EmptyTree =  singleton x
treeInsert x (Node a left right)
  | x == a  = Node a left right
  | x > a   = Node a left (treeInsert x right)
  | x < a   = Node a (treeInsert x left) right

treeElem :: (Ord a) => a -> Tree a -> Bool
treeElem x EmptyTree = False
treeElem x (Node a left right)
  | x == a  = True
  | x < a   = treeElem x left
  | x > a   = treeElem x right




main = do
    let a = [5,2,7,3,6,4,1]
    print $ foldr treeInsert EmptyTree a
