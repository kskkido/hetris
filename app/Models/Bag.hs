module Models.Bag
  ( Bag
  , pick
  , shuffle
  ) where

import Control.Monad.State
  ( State
  , evalState
  , get
  , put
  )

import System.Random
  ( StdGen
  , mkStdGen
  , randomR
  )

import Models.Shape
  ( Shape
  , shapes
  )

type Bag = State StdGen Shape

pick :: Bag -> StdGen -> Shape
pick = evalState

shuffle :: Bag
shuffle = get >>= unwrap . randomR (0, length shapes - 1)
  where unwrap (x,s) = put s >> return (shapes !! x)
