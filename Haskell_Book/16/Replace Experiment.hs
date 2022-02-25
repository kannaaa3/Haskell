module ReplaceExperiment where

replaceWithP :: b -> Char
replaceWithP = const 'p'

lms :: [Maybe String]
lms = [Just "Ave", Nothing, Just "woohoo"]

-- Just making the argument more specific
replaceWithP' :: [Maybe String] -> Char
replaceWithP' = replaceWithP

-- What happens if we lift it?
-- Prelude> :t fmap replaceWithP
-- fmap replaceWithP :: Functor f
-- => f a -> f Char
liftedReplace :: Functor f => f a -> f Char
liftedReplace = fmap replaceWithP

thriceLifted :: (Functor f, Functor f1, Functor f2) => f (f1 (f2 a)) -> f (f1 (f2 Char))
thriceLifted  = (fmap . fmap . fmap) replaceWithP

thriceLifted' :: [Maybe String] -> [Maybe String]
thriceLifted' = thriceLifted
