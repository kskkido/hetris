module Models.Matrix
  ( Matrix(..)
  , create
  , spawn
  , spawnable
  , command
  , commandable
  , clear
  , clearable
  , place
  , placeable
  , playable
  , hardDrop
  ) where

import qualified Models.Board as MB
  ( Board
  , create
  , place
  , placeable
  , remove
  , shift
  , fullRows
  , available
  )

import Models.Block
  ( Block(..)
  )

import Models.Cell
  ( Cell(..)
  , value
  , coordinate
  , anchor
  , convert
  )

import Models.Coordinate
  ( Coordinate
  )

import Models.Piece
  ( Piece
  , empty
  , moves
  , parse
  , moveD
  )

import Models.Shape
  ( Shape
  )

data Matrix = Matrix
  { board :: MB.Board
  , piece :: Piece
  } deriving (Show)

create :: Int -> Int -> Matrix
create x y = Matrix
  { board = MB.create x y
  , piece = empty
  }

clean :: Matrix -> Matrix
clean Matrix { board = b, piece = p } = Matrix
  { board = MB.remove (parse p) b
  , piece = empty
  }

clear :: Matrix -> Matrix
clear m = if clearable m then Matrix
  { board = MB.shift (MB.fullRows $ board m) (board m)
  , piece = piece m 
  } else m

clearable :: Matrix -> Bool
clearable m = not (null $ MB.fullRows $ board m)

spawn :: Shape -> Matrix -> Matrix
spawn t m = if spawnable t m then Matrix
  { board = MB.place (parse p) (board m)
  , piece = p
  } else m
  where p = ((0,0),t)

spawnable :: Shape -> Matrix -> Bool
spawnable t m = not (playable m) && placeable p m
  where p = ((0,0),t)

place :: Piece -> Matrix -> Matrix
place p m = if placeable p m then Matrix
  { board = MB.place (parse p) (board $ clean m)
  , piece = p
  } else m

placeable :: Piece -> Matrix -> Bool
placeable p = MB.placeable (parse p) . board . clean

command :: (Piece -> Piece) -> Matrix -> Matrix
command fn m = if commandable fn m then place (fn (piece m)) m else m

commandable :: (Piece -> Piece) -> Matrix -> Bool
commandable fn m = placeable (fn $ piece m) m

playable :: Matrix -> Bool
playable m = empty /= piece m && commandable moveD m

hardDrop :: Matrix -> Matrix
hardDrop m = if commandable moveD m then hardDrop (command moveD m) else m