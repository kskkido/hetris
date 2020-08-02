module Views.Matrix
  ( render
  , Matrix(..)
  ) where

import Graphics.Gloss
  ( Picture
  )

import Models.Matrix
  ( Matrix(..)
  )

import qualified Views.Board as PB
  ( render
  )

render :: (Float,Float) -> Matrix -> Picture
render d = PB.render d . board