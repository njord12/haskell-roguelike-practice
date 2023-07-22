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
        delta = [(newPlayerPos, Player), (oldPlayerPos, Floor)]
    in 
        (RenderBoard delta, newState)



moveHelper :: Movement -> PlayerData -> BoardInfo -> Point
moveHelper mov (PlayerData (y,x) _) (BoardInfo h w) =
    case mov of
        North -> (handleEdge (y - 1) h, x)
        South -> (handleEdge (y + 1) h, x)
        East -> (y, handleEdge (x + 1) w)
        West -> (y, handleEdge (x - 1) w)
        NorthEast -> (handleEdge (y - 1) h, handleEdge (x + 1) w)
        NorthWest -> (handleEdge (y - 1) h, handleEdge (x - 1) w)
        SouthEast -> (handleEdge (y + 1) h, handleEdge (x + 1) w)
        SouthWest -> (handleEdge (y + 1) h, handleEdge (x - 1) w)
        None -> (y, x)
    where
        handleEdge delta edge
            |delta < 1 = 1
            |delta > edge = edge
            |otherwise = delta
