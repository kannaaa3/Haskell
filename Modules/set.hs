import qualified Data.Set as Set

main = do
    let text1 = "concho"
        text2 = "aaaafashojs"
    let set1    = Set.fromList text1
        set2    = Set.fromList text2

    print $ Set.intersection set1 set2
    print $ Set.difference set1 set2 
    --print $ Set.difference set2 set1
    print $ Set.union set1 set2

    -- Set.null Set.empty == True
    -- Set.size, member, singleton, insert, delete
    print $ Set.filter odd $ Set.fromList [3,4,5,6,7,2,3,4]
    --let setNub = xs = Set.toList $ Set.fromList xs
        



