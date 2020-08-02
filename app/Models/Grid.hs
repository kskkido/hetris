module Models.Grid
  ( Grid(..)
  , cells
  , create
  , merge
  , omit
  , pick
  , put
  , update
  , has
  , within
  , rows
  , maxCol
  , maxRow
  ) where

import Data.List
  ( find
  , groupBy
  )

import Lib.Combinator
  ( on
  )

import Models.Cell
  ( Cell(..)
  , value
  , values
  , coordinate
  , coordinates
  , regroup
  , convert
  , deconvert
  , match
  , stream
  )

import Models.Coordinate
  ( Coordinate
  )

newtype Grid a = Grid [Cell a] deriving (Show)

instance Functor Grid where
  fmap fn (Grid cs) = Grid (map (fmap fn) cs)

cells :: Grid a -> [Cell a]
cells (Grid cs) = cs

create :: Int -> Int -> Grid ()
create x y = Grid (concatMap (take x) $ take y stream)

merge :: Grid a -> Grid a -> Grid a
merge g h | maxCol g == maxCol h = Grid (convert $ cg ++ ch)
          | otherwise            = h
  where cg = deconvert (cells g)
        ch = deconvert (cells h)

omit :: [Int] -> Grid a -> Grid a
omit rs g = Grid $ regroup cs
  where cs = filter (\c -> snd (coordinate c) `notElem` rs) (cells g)

pick :: [Coordinate] -> Grid a -> [Cell a]
pick cxs (Grid cs) = concatMap (\cx -> filter (match cx) cs) cxs

update :: Cell a -> Grid a -> Grid a
update c = foldr step (Grid []) . cells
  where step d | match (coordinate c) d = Grid . (c:) . cells
               | otherwise              = Grid . (d:) . cells

put :: [Cell a] -> Grid a -> Grid a
put cs g | has cs g = foldr update g cs
         | otherwise = g

has :: [Cell a] -> Grid a -> Bool
has cs g = all ((`within` g) . coordinate) cs

within :: Coordinate -> Grid a -> Bool
within (x,y) g = withinRow && withinCol
  where withinRow = 0 <= y && y <= maxRow g
        withinCol = 0 <= x && x <= maxCol g

rows :: Grid a -> [[a]]
rows = deconvert . cells

maxRow :: Grid a -> Int
maxRow (Grid cs) = foldr (max . snd) 0 (coordinates cs)

maxCol :: Grid a -> Int
maxCol (Grid cs) = foldr (max . fst) 0 (coordinates cs)
