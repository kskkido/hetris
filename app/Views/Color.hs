module Views.Color
  ( render
  , MC.Color(..)
  ) where

import qualified Graphics.Gloss as GG
  ( Color
  , red
  , blue
  , green
  , yellow
  , orange
  , violet
  , cyan
  , white
  , black
  )

import qualified Models.Color as MC
  ( Color(..)
  )

render :: MC.Color -> GG.Color
render MC.Red    = GG.red
render MC.Blue   = GG.blue
render MC.Green  = GG.green
render MC.Yellow = GG.yellow
render MC.Orange = GG.orange
render MC.Purple = GG.violet
render MC.Cyan   = GG.cyan
render MC.White  = GG.white
render MC.Black  = GG.black