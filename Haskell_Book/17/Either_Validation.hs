module Either_Validation where 

type E = Either
-- (<*>) :: f (a -> b) -> f a -> f b
-- (<*>) :: E e (a -> b) -> E e a -> E e b
-- pure :: a -> f a
-- pure :: a -> E e a

main = do
  print "The quick brown fox jumps over the lazy dog."
