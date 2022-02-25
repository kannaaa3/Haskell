myIterate :: (a -> a) -> a -> [a]
myIterate f x = x : (myIterate f (f x))



myUnfoldr :: (b -> Maybe (a, b))
          -> b
          -> [a]
myUnfoldr f x = fstVal : (myUnfoldr f sndVal)
    where Just (fstVal, sndVal) = f x

betterIterate :: (a -> a) -> a -> [a]
betterIterate f x = myUnfoldr f' x
    where f' x = Just (f x, f x)

data BinaryTree a = Leaf 
                  | Node (BinaryTree a) a (BinaryTree a)
                  deriving (Show)

singletonTree :: a -> BinaryTree a
singletonTree x = Node Leaf x Leaf

insertTree :: (Ord a) => a -> BinaryTree a -> BinaryTree a
insertTree x Leaf   = singletonTree x
insertTree x (Node lft mid rgt) 
  | x == mid    = Node lft mid rgt
  | x < mid     = Node (insertTree x lft) mid rgt
  | x > mid     = Node lft mid (insertTree x rgt)

unfold :: (Ord b) => (a -> Maybe (a, b, a))
        -> a
        -> BinaryTree b
unfold f x = case  f x of
               Nothing          -> Leaf
               Just (x, y, z)   -> Node (unfold f x) y (unfold f z)

treeBuild :: Integer -> BinaryTree Integer
treeBuild n = unfold integerToMaybe 0
    where integerToMaybe x
            | x == n    = Nothing
            | otherwise = Just (x + 1, x , x + 1)

treeBuild' :: Integer -> BinaryTree Integer
treeBuild' n = unfold integerToMaybe 0
    where integerToMaybe x  = Just (x + 1, x , x + 1)
          integerToMaybe n  = Nothing   --





