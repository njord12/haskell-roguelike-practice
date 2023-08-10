module DungeonGenerator
where
import Types (Point, Cell(..), GridSize(..))
import RandomGenerator
import System.Random
import Entities (wallTile, floorTile)

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
        room1 = mkRectangularRoom (2,2) 4 4
        room2 = mkRectangularRoom (2,8) 3 3
    in
        roomToCellList room1 <> roomToCellList room2