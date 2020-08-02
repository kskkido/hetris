module Views.Block
  ( render
  , Block(..)
  ) where

import qualified Graphics.Gloss as GG
  ( Picture
  , Color
  , color
  , pictures
  , rectangleWire
  , rectangleSolid
  )

import Models.Block
  ( Block(..)
  )

import qualified Views.Color as PC
  ( Color(..)
  , render
  )

render :: (Float,Float) -> Block -> GG.Picture
render d b = GG.pictures [renderContent d b, renderOutline d b]

renderContent :: (Float,Float) -> Block -> GG.Picture
renderContent (w,h) b = GG.color blockColor blockShape
  where blockColor  = PC.render (color b)
        blockShape  = GG.rectangleSolid w h

renderOutline :: (Float,Float) -> Block -> GG.Picture
renderOutline (w,h) _ = GG.color blockColor blockShape
  where blockColor  = PC.render PC.Black
        blockShape  = GG.rectangleWire w h