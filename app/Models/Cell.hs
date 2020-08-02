module Models.Cell
  ( Cell(..)
  , value
  , values
  , coordinate
  , coordinates
  , match
  , stream
  , adjust
  , anchor
  , regroup
  , convert
  , deconvert
  ) where

import Data.List
  ( groupBy
  , sortBy
  )

import Lib.Combinator
  ( on
  )

import qualified Models.Coordinate as MC
  ( Coordinate
  , add
  , same
  , sameRow
  , stream
  )

newtype Cell a = Cell (MC.Coordinate, a) deriving (Show)

instance Functor Cell where
  fmap fn (Cell (cx,x)) = Cell (cx,fn x)

value :: Cell a -> a
value (Cell (_, x)) = x;

values :: [Cell a] -> [a]
values = map value

coordinate :: Cell a -> MC.Coordinate
coordinate (Cell (cx, _)) = cx;

coordinates :: [Cell a] -> [MC.Coordinate]
coordinates = map coordinate

match :: MC.Coordinate -> Cell a -> Bool
match cx (Cell (cy,_)) = MC.same cx cy

stream :: [[Cell ()]]
stream = map (map $ \cx -> Cell (cx, ())) MC.stream

adjust :: MC.Coordinate -> Cell a -> Cell a
adjust cx c = Cell (cx `MC.add` coordinate c, value c)

anchor :: MC.Coordinate -> [Cell a] -> [Cell a]
anchor = map . adjust

regroup :: [Cell a] -> [Cell a]
regroup = convert . deconvert

convert :: [[a]] -> [Cell a]
convert xss = concat $ zipWith (zipWith zipper) xss stream
  where zipper x c = fmap (const x) c

deconvert :: [Cell a] -> [[a]]
deconvert = map values . groupBy (coordinate `on` MC.sameRow)

sort :: [Cell a] -> [Cell a]
sort = sortBy (coordinate `on` compare)
