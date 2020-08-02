module Models.Coordinate
  ( Coordinate
  , add
  , same
  , sameRow
  , sameCol
  , stream
  ) where

type Coordinate = (Int, Int)

add :: Coordinate -> Coordinate -> Coordinate
add (x,y) (x',y') = (x + x', y + y')

same :: Coordinate -> Coordinate -> Bool
same cx cy = sameRow cx cy && sameCol cx cy

sameCol :: Coordinate -> Coordinate -> Bool
sameCol (x,_) (x',_) = x == x'

sameRow :: Coordinate -> Coordinate -> Bool
sameRow (_,y) (_,y') = y == y'

stream :: [[Coordinate]]
stream = rows
  where rows = map cols [0..]
        cols = zip [0..] . repeat
