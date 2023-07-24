module GameState (move, moveHelper)
where
import Types
import Data.Array ((//))


--      Test this
move :: BoardInfo -> Movement -> GameState -> GameState
move bi mov state@(GameState player grid) =
    let
        oldPlayerPos = position player
        newPlayerPos = moveHelper mov player bi
        newPlayerData = PlayerData newPlayerPos (hitPoints player)
        newState = state{player = newPlayerData, mapData = grid}
        delta = makeDelta oldPlayerPos newPlayerPos
    in
        updateMap newState delta
    where
        makeDelta oldPos newPos =
            if oldPos == newPos
            then
                [(oldPos, Player)]
            else
                [(newPos, Player), (oldPos, Floor)]

moveHelper :: Movement -> PlayerData -> BoardInfo -> Point
moveHelper mov (PlayerData (y,x) _) (BoardInfo h w) =
    case mov of
        North -> (handleEdge (y - 1) h, x)
        South -> (handleEdge (y + 1) h, x)
        East -> (y, handleEdge (x + 1) w)
        West -> (y, handleEdge (x - 1) w)
        NorthEast -> handleDiagonal y x (y - 1) (x + 1) h w
        NorthWest -> handleDiagonal y x (y - 1) (x - 1) h w
        SouthEast -> handleDiagonal y x (y + 1) (x + 1) h w
        SouthWest -> handleDiagonal y x (y + 1) (x - 1) h w
        None -> (y, x)
    where
        handleEdge delta edge
            |delta < 1 = 1
            |delta > edge = edge
            |otherwise = delta
        handleDiagonal currentY currentX deltaY deltaX yEdge xEdge
            |deltaY < 1 || deltaX < 1 = (currentY, currentX)
            |deltaY > yEdge ||deltaX > xEdge = (currentY, currentX)
            |otherwise = (deltaY, deltaX)


updateMap :: GameState -> DeltaBoard -> GameState
updateMap state@(GameState _ mapData) delta =
    let
        newGrid = grid mapData // delta
        newMapData = MapData (level mapData) (stairsPosition mapData) newGrid
    in
        state{mapData = newMapData}
