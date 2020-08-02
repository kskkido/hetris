module Views.Board
  ( render
  , Board
  ) where

import Graphics.Gloss
  ( Picture
  , pictures
  , translate
  )

import Models.Board
  ( Board
  )

import Models.Cell
  ( Cell(..)
  , value
  , coordinate
  )

import Models.Grid
  ( cells
  )

import qualified Views.Block as PB
  ( render
  , Block(..)
  )

render :: (Float,Float) -> Board -> Picture
render d = pictures . map (renderCell d) . cells

renderCell :: (Float,Float) -> Cell PB.Block -> Picture
renderCell (w,h) (Cell ((x,y),bl)) = translate offsetX offsetY (PB.render (w,h) bl)
  where offsetX = fromIntegral x * w
        offsetY = fromIntegral (-y) * h