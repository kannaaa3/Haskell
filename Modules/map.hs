--import qualified Data.List (lookup)
import qualified Data.Map as Map 

phoneBooktoMap :: (Ord k) => [(k, a)] -> Map.Map k [a]
phoneBooktoMap xs = Map.fromListWith (++) $ map (\(k,v) -> (k, [v])) xs

main = do 
    -- Map.fromList []              &&               Map.toList
    let k =  Map.insert 5 600 . Map.insert 4 200 . Map.insert 3 100 $ Map.empty
    -- Map.singleton 3 9                         &&               print $ Map.size k
    print $ Map.lookup 4 k -- Map.member 3 $ k
    let j = Map.fromList [(1,'a'), (2,'A'), (3, 'b'), (4, 'B')]
    print $ Map.filter (`elem` ['A'..'Z']) $ j                    -- print $ Map.map (^2) k
    --print $ Map.delete 4 k
    --print $ Map.adjust (+69) 4 k
    print $ Map.keys k
    --from List With
    print $ Map.insertWith (+) 4 100 k
