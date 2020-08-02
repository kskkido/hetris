module Models.Piece
  ( Piece
  , empty
  , moves
  , moveL
  , moveR
  , moveD
  , rotate
  , parse
  ) where

import Models.Block
  ( Block(..)
  )

import Models.Cell
  ( Cell(..)
  , convert
  , anchor
  )

import Models.Coordinate
  ( Coordinate
  , add
  )

import qualified Models.Shape as MS
  ( Shape
  , empty
  , rotate
  )

type Piece = (Coordinate, MS.Shape)

empty :: Piece
empty = ((0,0), MS.empty)

moves :: Piece -> [Piece]
moves p =
  [ moveL p
  , moveR p
  , moveD p
  , rotate p
  ]

move :: Coordinate -> Piece -> Piece
move cx (cy,t) = (cx `add` cy, t)

moveL :: Piece -> Piece
moveL = move (-1,0)

moveR :: Piece -> Piece
moveR = move (1,0)

moveD :: Piece -> Piece
moveD = move (0,1)

rotate :: Piece -> Piece
rotate (cx,t) = (cx, MS.rotate t)

parse :: Piece -> [Cell Block]
parse (cx,t) = anchor cx (convert t)