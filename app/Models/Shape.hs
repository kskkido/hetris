module Models.Shape
  ( Shape
  , shapes
  , shapeO
  , shapeI
  , shapeL
  , shapeJ
  , shapeS
  , shapeZ
  , shapeT
  , empty
  , rotate
  ) where

import Data.List
  ( transpose
  )

import qualified Models.Block as MB
  ( Block(..)
  , empty
  )

import Models.Color
  ( Color(..)
  )

import Models.Coordinate
  ( Coordinate
  )

type Shape = [[MB.Block]]

shapes :: [Shape]
shapes =
  [ shapeO
  , shapeI
  , shapeL
  , shapeJ
  , shapeS
  , shapeZ
  , shapeT
  ]

shapeO :: Shape
shapeO =
  [ [b, b]
  , [b, b]
  ]
  where b = MB.Block Yellow
        e = MB.empty

shapeI :: Shape
shapeI =
  [ [e, e, e, e]
  , [b, b, b, b]
  , [e, e, e, e]
  , [e, e, e, e]
  ]
  where b = MB.Block Cyan
        e = MB.empty

shapeL :: Shape
shapeL =
  [ [e, b, e]
  , [e, b, e]
  , [e, b, b]
  ]
  where b = MB.Block Orange
        e = MB.empty

shapeJ :: Shape
shapeJ =
  [ [e, b, e]
  , [e, b, e]
  , [b, b, e]
  ]
  where b = MB.Block Blue
        e = MB.empty

shapeS :: Shape
shapeS =
  [ [e, b, b]
  , [b, b, e]
  , [e, e, e]
  ]
  where b = MB.Block Red
        e = MB.empty

shapeZ :: Shape
shapeZ =
  [ [b, b, e]
  , [e, b, b]
  , [e, e, e]
  ]
  where b = MB.Block Green
        e = MB.empty

shapeT :: Shape
shapeT = 
  [ [e, b, e]
  , [b, b, b]
  , [e, e, e]
  ]
  where b = MB.Block Purple
        e = MB.empty

empty :: Shape
empty = []

rotate :: Shape -> Shape
rotate = map reverse . transpose