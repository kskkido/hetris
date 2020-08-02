module Models.Block
  ( Block(..)
  , tangible
  , empty
  ) where

import Models.Color
  ( Color(..)
  )

data Block = Block
  { color :: Color
  }
  | Empty
  { color :: Color
  }
  deriving (Show, Eq)

tangible :: Block -> Bool
tangible (Block _) = True
tangible _         = False

empty :: Block
empty = Empty White