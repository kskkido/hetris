module Lib.Combinator
  ( (...)
  , on
  ) where

(...) :: (c -> d) -> (a -> b -> c) -> a -> b -> d
(...) = (.) . (.)

on :: (a -> b) -> (b -> b -> c) -> a -> a -> c
on f g x y = f x `g` f y