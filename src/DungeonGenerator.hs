module DungeonGenerator
where
import Types (Point, Cell(..), GridSize(..))
import RandomGenerator
import System.Random
import Entities (wallTile, floorTile)
import Data.List (unfoldr, sort)

data RectangularRoom = RectangularRoom {corner :: Point,corner2 :: Point, height :: Int, width :: Int} deriving Show

mkRectangularRoom :: Point -> Int -> Int -> RectangularRoom
mkRectangularRoom (y, x) h w = RectangularRoom (y, x) (y + h - 1,x + w - 1) h w

center :: RectangularRoom -> Point
center (RectangularRoom (y1,x1) (y2,x2) _ _) = ((y1 + y2) `div` 2, (x1 + x2) `div` 2)

innerRoom :: RectangularRoom -> [Point]
innerRoom (RectangularRoom (y1, x1) (y2,x2) _ _) =
    let
        ySlice = [y1 + 1 .. y2 - 1]
        xSlice = [x1 + 1 .. x2 - 1]
    in
        [(y,x) | y <- ySlice, x <- xSlice]

roomToCellList :: RectangularRoom -> [(Point, Cell)]
roomToCellList room@(RectangularRoom (y1, x1) (y2, x2) _ _) =
    let
        edge = [(y,x) | y <- [y1 .. y2], x <- [x1 .. x2]]
        inside = innerRoom room
        walls = [(p, Cell Nothing wallTile) | p <- edge ]
        floors = [(p, Cell Nothing floorTile) | p <- inside]
    in
        walls <> floors

makeDungeon :: GridSize -> [(Point, Cell)]
makeDungeon (GridSize h w) =
    let
        room1 = mkRectangularRoom (2,2) 6 6
        room2 = mkRectangularRoom (11,2) 4 5
        path = [(p, Cell Nothing floorTile) | p <- line (center room1) (center room2)] 
    in
        roomToCellList room1 <> roomToCellList room2 <> path

line :: Point -> Point -> [Point]
line pa@(xa,ya) pb@(xb,yb) = map maySwitch . unfoldr go $ (x1,y1,0)
  where
    steep = abs (yb - ya) > abs (xb - xa)
    maySwitch = if steep then (\(x,y) -> (y,x)) else id
    [(x1,y1),(x2,y2)] = sort [maySwitch pa, maySwitch pb]
    deltax = x2 - x1
    deltay = abs (y2 - y1)
    ystep = if y1 < y2 then 1 else -1
    go (xTemp, yTemp, error)
        | xTemp > x2 = Nothing
        | otherwise  = Just ((xTemp, yTemp), (xTemp + 1, newY, newError))
        where
          tempError = error + deltay
          (newY, newError) = if (2*tempError) >= deltax
                            then (yTemp+ystep,tempError-deltax)
                            else (yTemp,tempError)

bresenhamLine :: Point -> Point -> [Point]
bresenhamLine p1 p2 = map swap (line p1 p2)

swap :: Point -> Point
swap (x,y) = (y,x)