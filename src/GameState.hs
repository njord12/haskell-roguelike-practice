module GameState (move,initializeMap, getEntityPosition)
where
import Types
import Entities (makePlayer, floorTile, wallTile)
import Data.Array ( (//), listArray, (!) )
import Data.Array.Base (assocs)
import Data.Maybe (fromMaybe)
import RandomGenerator (PCGen(PCGen))
import DungeonGenerator (makeDungeon)


--Test this
--This might cause problems if the entity doesn't exist 
--Will most likely need to refactor it
move :: GridSize -> Movement -> Entity -> GameState -> GameState
move bi mov entity state@(GameState mapGrid gen) =
    let
        oldPosition = fromMaybe (0, 0) (getEntityPosition entity state)
        newPosition = moveHelper mov oldPosition bi
        currentCell = grid mapGrid ! oldPosition
        newCell = grid mapGrid ! newPosition
        delta = [(oldPosition, currentCell{entity = Nothing}), (newPosition, newCell{entity=Just entity})]
    in
        if isWalkable newPosition (grid mapGrid)
            then
                case oldPosition of
                    (0, 0) -> state
                    (_, _) -> updateMap state delta
            else
                state



moveHelper :: Movement -> Point -> GridSize -> Point
moveHelper mov (y,x) (GridSize h w) =
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

initializeMap :: GridSize -> GameState
initializeMap gridSize@(GridSize h w) =
    let
        playerEntity = makePlayer
        iniMap = listArray ((1,1), (h, w)) (replicate (h * w) (Cell Nothing wallTile))
        mapWithRooms = iniMap // makeDungeon gridSize
        initialPlayerPos = findEmptyPoint (assocs mapWithRooms)
        mapWithPlayer = mapWithRooms // [(fromMaybe (1,1) initialPlayerPos, Cell (Just playerEntity) floorTile)] 
        mData = MapData 1 mapWithPlayer
    in
        GameState mData (PCGen 10000 10000)


updateMap :: GameState -> GridDelta -> GameState
updateMap state@(GameState mapData gen) delta =
    let
        newGrid = grid mapData // delta
        newMapData = MapData (level mapData) newGrid
    in
        state{mapData = newMapData}


getEntityPosition :: Entity -> GameState -> Maybe Point
getEntityPosition entity (GameState mapData gen) =
    let
        elements = assocs $ grid mapData
        point = findEntity entity elements
    in
        point

findEntity :: Entity -> [(Point, Cell)] -> Maybe Point
findEntity _ [] = Nothing
findEntity entity ((p, Cell (Just e) _ ) : es) = if entity == e then Just p else findEntity entity es
findEntity entity ((_, Cell Nothing _) : es) = findEntity entity es

isWalkable :: Point -> Grid -> Bool
isWalkable p grid =
    let
        (Cell e t) = grid ! p
        isThereEntity = 
            case e of
                Just x -> True
                Nothing -> False
    in
        not (blocksMovement t || isThereEntity)

findEmptyPoint :: [(Point, Cell)] -> Maybe Point
findEmptyPoint [] = Nothing
findEmptyPoint ((p, Cell (Just e) t):cs) = findEmptyPoint cs
findEmptyPoint ((p,Cell Nothing t):cs) =
   if not $ blocksMovement t 
   then Just p
   else
    findEmptyPoint cs

