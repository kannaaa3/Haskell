{-# LANGUAGE FlexibleInstances #-}
import Data.Kind
import GHC.Arr
a = fmap (+1) $ read "[1]" :: [Int]
b = (fmap . fmap) (++ "lol") (Just ["Hi, ", "Hello"])
c = fmap (*2) (\x -> x - 2)

d :: Monad m => Int -> m [Char] 
d = fmap ("1"++) . return . show . (\x -> [x, (1 :: Int)..3])

-- e = [1, 2, 3] >>= d
-- ee = (Just 3) >>= d


e :: IO Integer
e = let ioi     = read "1" ::  Integer
        changed = readIO ("123" ++ show ioi) :: IO Integer
      in  fmap (*3) changed

------------------------------------------------

data Four a b c d = Four a b c d

instance Functor (Four a b c) where
  fmap f (Four a b c d) = Four a b c (f d)

data Four' a b = Four' a a a b deriving (Show, Eq)

instance Functor (Four' a) where
  fmap f (Four' x y z b) = Four' x y z (f b)

------------------------------------------------------




data BoolAndmaybeSomethingElse a = 
  Falsish | Truish a

instance Functor BoolAndmaybeSomethingElse where
  fmap _ Falsish = Falsish
  fmap f (Truish a) = Truish (f a)



newtype Mu f = InF { outF :: f (Mu f)}

-- instance Functor (Mu Int) where
  -- fmap f (InF x) = Inf $ f x

data D = 
  D (Array Word Word) Int Int


data More a b = L a b a | R b a b deriving (Eq, Show)
instance Functor (More x) where
  fmap f (L a b a') = L a (f b) a'
  fmap f (R b a b') = R (f b) a (f b')

------------------------------------  
data Quant a b = Finance | Desk a | Bloor b deriving (Eq, Show)

instance Functor (Quant a) where
  fmap f (Bloor b) = Bloor $ f b
  fmap _ (Desk x)         = Desk x
  fmap _ Finance          = Finance

newtype K a b = 
  K a
  deriving (Eq,Show)

instance Functor (K a) where
  fmap f (K a)  = K a

newtype Flip f a b =
  Flip (f b a)
  deriving (Eq, Show)

instance Functor (Flip K a) where
  fmap f (Flip (K a )) = Flip $ K (f a)

----------------------------------------

newtype EvilGoateeConst a b =
  GoatyConst b
  deriving (Eq, Show)

instance Functor (EvilGoateeConst a) where
  fmap f (GoatyConst b) = GoatyConst $ f b
----------------------------------------------

newtype LiftItOut f a = 
  LiftItOut (f a)

instance Functor f => Functor (LiftItOut f) where
  fmap f (LiftItOut fa)  = LiftItOut $ fmap f fa

data Parappa f g a =
  DaWrappa (f a) (g a)

instance (Functor f, Functor g) =>  Functor (Parappa f g) where
  fmap f (DaWrappa fa ga) = DaWrappa (fmap f fa) (fmap f ga)

data IgnoreOne f g a b =
  IgnoringSomething (f a) (g b)

instance  Functor g => Functor (IgnoreOne f g a ) where
  fmap f (IgnoringSomething fa gb) = IgnoringSomething fa $ fmap f gb

data List a = 
    Nil 
  | Cons a (List a)
  deriving (Eq, Show)

instance Functor List where
  fmap f Nil  = Nil
  fmap f (Cons a xs)  = Cons (f a) (fmap f xs)

data GoatLord a = 
    NoGoat
  | OneGoat a
  | MoreGoats (GoatLord a) (GoatLord a) (GoatLord a)
  deriving (Eq, Show)
-- 713
instance Functor GoatLord where   
  fmap _ NoGoat = NoGoat
  fmap f (OneGoat a)  = OneGoat $ f a
  fmap f (MoreGoats x y z) = MoreGoats (fmap f x) (fmap f y) (fmap f z)



-- instance Functor ((->) String) where
--   fmap = (.)

data TalkToMe a = 
    Halt
  | Print String a
  | Read (String -> a)

instance Functor TalkToMe where
  fmap _ Halt = Halt
  fmap f (Print x a) = Print x (f a)
  fmap f ((Read g) ) = Read (fmap f g)
