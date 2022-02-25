fu :: (Num a) => [a] -> [a] -> [a]
fu a b = zipWith mf a b
    where 
        mf x y = x + (db y)
        db y = 2 * y


main = do
    print $ fu [7] [8]

