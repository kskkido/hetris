module Services.Game.Frame
  ( next
  ) where

import System.Random
  ( mkStdGen
  )

import Models.Bag
  ( pick
  , shuffle
  )

import Models.Matrix
  ( Matrix(..)
  , clear
  , clearable
  , spawn
  , spawnable
  , command
  , playable
  )

import Models.Piece
  ( moveD
  )

import Models.Shape
  ( shapeL
  )

import Models.Game
  ( Game
  , tick
  , eval
  , frame
  )

next :: Float -> Game -> Game
next f g = tick (step $ round f + frame g) g

step :: Int -> Matrix -> Matrix
step i m = if spawnable p n then spawn p n else n
 where n = if playable o then command moveD o else o
       o = if clearable m then clear m else m
       p = pick shuffle (mkStdGen i)