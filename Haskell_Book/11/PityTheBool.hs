module PityTheBool where

import Data.Int

data BigSmall =
    Big Bool
  | Small Bool
  deriving (Eq, Show)

data NumberOrBool =
    Numba Int8
  | BoolyBool Bool
  deriving (Eq, Show)
-- parentheses due to syntactic
-- collision between (-) minus
-- and the negate function
let myNumba = Numba (-128)
