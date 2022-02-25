import Data.List --(nub, sort)
import qualified Data.Map as M -- dealing with name clashes
-- import Data.List hiding (nub)




main = do
    let a = "8881837737371863"
        f = length . nub 
    print $ f . tail $ a 
    --        (length(nub(tail a)))
    
    -- foldl' and  foldl1' : strict version, avoid stack overflow, a thunk

    -- Data.List

    let j   = intersperse '.' "concho" -- "c.o.n.c.h.o"
        k   = intercalate " con meo " ["con", "cho", "1"]
        x   = transpose [[1, 2, 3], [4, 5, 6], [7, 8, 9]]
        k0  = map sum $ transpose [[1, 2, 3], [4, 5, 6], [7, 8, 9]]
    print k0

    let aa = concat [[[x] | x <- [1..3]]] --rm 1 level of nesting / concat twice
        a' =   concatMap (map (map (^2))) [[[x]] | x <- [1..3] ]
        rac = or $ map (==4) [2, 3, 4, 5, 5] -- or $ map (>4) [1,2,3 ]
        
    -- all (==4) [2,4]
    -- any (==4) [2,4]
    -- take 10 $ iterate (*2) 1
    -- f = take 10 . iterate (*2)  &&   take 10 $ f 1
    -- splitAt 3 "heyman"
    -- takeWhile >< dropWhile
    -- let (fw, rest) = span (/=' ') "con cho" in fw ++ rest
    -- break = span (not . p)
    -- group [1,1,3,2]  -> [[1,1], [3], [2]]
    print a'



