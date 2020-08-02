module Models.Board
  ( Board
  , create
  , place
  , placeable
  , remove
  , available
  , shift
  , fullRows
  ) where

import Lib.Combinator
  ( (...)
  )

import Models.Block
  ( Block(..)
  , tangible
  , empty
  )

import Models.Coordinate
  ( Coordinate
  )

import qualified Models.Cell as MC
  ( Cell(..)
  , value
  , coordinate
  , coordinates
  )

import qualified Models.Grid as MG
  ( Grid(..)
  , create
  , cells
  , has
  , merge
  , omit
  , pick
  , put
  , rows
  , update
  , maxCol
  )

type Board = MG.Grid Block

create :: Int -> Int -> Board
create = fmap (const empty) ... MG.create

shift :: [Int] -> Board -> Board
shift rs b = MG.merge h g
  where g  = MG.omit rs b
        h  = create (MG.maxCol b + 1) (length rs)

place :: [MC.Cell Block] -> Board -> Board
place cs b | not (available bs b) = MG.put bs b
           | otherwise           = b
  where bs = blocks cs

placeable :: [MC.Cell Block] -> Board -> Bool
placeable cs b = MG.has bs b && not (available cs b)
  where bs     = blocks cs

available :: [MC.Cell Block] -> Board -> Bool
available cs b = null bs || any (tangible . MC.value) bs
  where bs     = MG.pick (MC.coordinates $ blocks cs) b

remove :: [MC.Cell Block] -> Board -> Board
remove = MG.put . map (\c -> MC.Cell (MC.coordinate c, empty)) . blocks

blocks :: [MC.Cell Block] -> [MC.Cell Block]
blocks = filter (tangible . MC.value)

fullRows :: Board -> [Int]
fullRows b = map fst $ filter pred $ zip [0..] (MG.rows b)
  where pred (_,xs) = empty `notElem` xs
