
ans :: Int -> Int
ans = go 0 1 1
    where
        go acc x2 x1 1 = acc + (if mod x1 2 == 0 then x1 else 0)
        go acc x2 x1 num = acc `seq` go (acc + (if mod x1 2 == 0 then x1 else 0)) x1 (x2 + x1) (num - 1)

main = do
    print $ ans 300
