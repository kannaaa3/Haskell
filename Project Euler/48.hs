binpow :: Integer -> Integer -> Integer 
binpow _ 0 = 1
binpow n k = 
    let u   = k `div` 2
        _T  = binpow n u `mod` 10000000000
        j = (_T * _T) `mod` 10000000000
        concho = if even k then j else (j * n) `mod` 10000000000
    in concho

cho :: Integer -> Integer
cho 1 = 1
cho n = (binpow n n) + ( cho (n - 1)) `mod` 10000000000

main = do
    print $ cho 1000
