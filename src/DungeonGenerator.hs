module DungeonGenerator
where
import Types (Point)
import RandomGenerator
import System.Random

data RectangularRoom = RectangularRoom {corner :: Point,corner2 :: Point, height :: Int, width :: Int}

mkRectangularRoom :: Point -> Int -> Int -> RectangularRoom
mkRectangularRoom (y, x) h w = RectangularRoom (y, x) (y + h,x + w) h w

center :: RectangularRoom -> Point
center (RectangularRoom (y1,x1) (y2,x2) _ _) = ((y1 + y2) `div` 2, (x1 + x2) `div` 2)

innerRoom :: RectangularRoom -> ([Int], [Int])
innerRoom (RectangularRoom (y1, x1) (y2,x2) _ _) =
    let
        ySlice = [y1 + 1 .. y2]
        xSlice = [x1 + 1 .. x2]
    in
        (ySlice, xSlice)
