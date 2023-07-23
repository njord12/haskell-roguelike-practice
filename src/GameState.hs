module GameState
where
import Types


--Todo: Movement should probably come from outside instead of being part of game state
--      Test this
move :: BoardInfo -> GameState -> (RenderMessage, GameState)
move bi state@(GameState player grid mov) = 
    let
        oldPlayerPos = position player
        newPlayerPos = moveHelper mov player bi
        newPlayerData = PlayerData newPlayerPos (hitPoints player)
        newState = state{player = newPlayerData, mapData = grid, movement = None}
        delta = makeDelta oldPlayerPos newPlayerPos
    in 
        (RenderBoard delta, newState)
    where
        makeDelta oldPos newPos =
            if oldPos == newPos
            then
                [(oldPos, Player)]
            else
                [(newPos, Player), (oldPos, Floor)]


--TODO: 
-- moving diagonally on an edge but not corner moves laterally
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
