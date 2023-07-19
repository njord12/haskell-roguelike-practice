module GameState
where
import RenderState (Point, RenderMessage, BoardInfo (..))


data Movement = North | South | East | West | NorthEast | NorthWest | SouthEast | SouthWest

data Player = Player {postition :: Point, hitPoints :: Int} deriving (Show, Eq)

data MapData = MapData {level :: Int, stairsPosition :: Point}

data GameState = GameState {player :: Player, mapData :: MapData, movement :: Movement}


-- move :: BoardInfo -> GameState -> (RenderState.RenderMessage, GameState)
-- move bi (GameState player map mov) = 
--     let


moveHelper :: Movement -> Player -> BoardInfo -> Point
moveHelper mov (Player (y,x) _) (BoardInfo h w) =
    case mov of
        North -> (y - 1, x)
        South -> (y + 1, x)
        East -> (y, x + 1)
        West -> (y, x - 1)
        NorthEast -> (y - 1, x + 1)
        NorthWest -> (y - 1, x - 1)
        SouthEast -> (y + 1, x + 1)
        SouthWest -> (y + 1, x - 1)
