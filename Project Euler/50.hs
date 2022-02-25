import Control.Parallel
import qualified Data.Map as Map

sieve :: [Int] -> [Int]
sieve [] = []
sieve (x : xs) = x : sieve [n | n <- xs, n `mod` x /= 0]

initM :: [Int] -> [(Int, Int)]
initM (x : xs) 
  | xs == []    = []
  | otherwise   = (x, head xs) : initM xs



solve primemap leftp rightp currentsum currentval cnt 
    | currentsum == currentval  =  
        max cnt $ solve primemap leftp nextrightp (currentsum + nextrightp)  
    | currentsum > currentval   = 
        let just nextleftp   = map.lookup leftp primemap
        in solve primemap nextleftp rightp (currentsum - leftp) currentval (cnt - 1)
    | otherwise                 =   
        if rightp == 199999
           then cnt
           else solve primemap leftp (map. )


main = do
    let primeList   = sieve [2..100]
        primeMap    = Map.fromList $ initM primeList
        --primeMap =
    
    

    
    print $ primeMap
    
