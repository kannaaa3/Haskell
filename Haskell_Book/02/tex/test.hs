module Learn where 

sayHello :: String ->  IO ()
sayHello s = print("Hi, " ++ s ++ "!")

foo x = 
  let y = x * 2 
      z = x ^ 2 
  in 2 * y * z

------------------------------ Let and Where -------------------------------------

printInc n = print plusTwo
  where plusTwo = n + 2

printInx n = let plusTwo = n + 2
             in  print plusTwo
