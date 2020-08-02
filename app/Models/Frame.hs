module Models.Frame
  ( Frame(..)
  , create
  , tick
  , eval
  , frame
  ) where

newtype Frame a = Frame (a,Int)

create :: a -> Frame a
create x = Frame (x,0)

tick :: (a -> b) -> Frame a -> Frame b
tick fn (Frame (x,f)) = Frame (fn x, f + 1)

eval :: Frame a -> a
eval (Frame (x,_)) = x

frame :: Frame a -> Int
frame (Frame (_,x)) = x