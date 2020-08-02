module Views.Game
  ( render
  ) where 

import Graphics.Gloss
  ( Picture
  )

import Models.Game
  ( Game
  , eval
  )

import qualified Views.Matrix as PM
  ( render
  )

render :: (Float,Float) -> Game -> Picture
render d = PM.render d . eval
