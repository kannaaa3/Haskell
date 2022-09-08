{-# LANGUAGE NoMonomorphismRestriction #-}

module DetermineTheType where

-- import typeInference1

-- simple example
examples = 1

x = print
z = x "hello"

idH (x:_) = x
