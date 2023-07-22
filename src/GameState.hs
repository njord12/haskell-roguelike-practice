module GameState
where
import RenderState (BoardInfo (BoardInfo), Point)


data Movement = North | South | East | West | NorthEast | NorthWest | SouthEast | SouthWest deriving Show

data Player = Player {postition :: Point, hitPoints :: Int} deriving (Show, Eq)

data MapData = MapData {level :: Int, stairsPosition :: Point}

data GameState = GameState {player :: Player, mapData :: MapData, movement :: Movement}


-- move :: BoardInfo -> GameState -> (RenderState.RenderMessage, GameState)
-- move bi (GameState player map mov) = 
--     let


moveHelper :: Movement -> Player -> BoardInfo -> Point
moveHelper mov (Player (y,x) _) (BoardInfo h w) =
    case mov of
        North -> (handleEdge (y - 1) h, x)
        South -> (handleEdge (y + 1) h, x)
        East -> (y, handleEdge (x + 1) w)
        West -> (y, handleEdge (x - 1) w)
        NorthEast -> (handleEdge (y - 1) h, handleEdge (x + 1) w)
        NorthWest -> (handleEdge (y - 1) h, handleEdge (x - 1) w)
        SouthEast -> (handleEdge (y + 1) h, handleEdge (x + 1) w)
        SouthWest -> (handleEdge (y + 1) h, handleEdge (x - 1) w)
    where
        handleEdge delta edge
            |delta < 1 = 1
            |delta > edge = edge
            |otherwise = delta
