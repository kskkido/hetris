module Models.Game
  ( Game
  , put
  , create
  , MF.tick
  , MF.eval
  , MF.frame
  ) where

import Lib.Combinator
  ( (...)
  )

import qualified Models.Frame as MF
  ( Frame(..)
  , create
  , tick
  , eval
  , frame
  )

import qualified Models.Matrix as MM
  ( Matrix(..)
  , create
  )

type Game = MF.Frame MM.Matrix

create :: Int -> Int -> Game
create = MF.create ... MM.create

put :: (MM.Matrix -> MM.Matrix) -> Game -> Game
put fn (MF.Frame (x,i)) = MF.Frame (fn x,i)