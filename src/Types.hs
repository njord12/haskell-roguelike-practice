module Types (Movement (..),
    MapData (..),
    GameState(..),
    Point,
    TileType (..),
    GridSize (..),
    Board,
    DeltaBoard,
    RenderMessage (..),
    RenderState (..),
    PlayerData (..),
    Tile (..),
    Entity (..),
    EntityType (..),
    Cell (..),
    GridDelta,
    Grid,
    emptyMessageQueue,
    enqueueMessage,
    nextMessage 
    )
where
import Data.Array ( Array )
import RandomGenerator

newtype MessageQueue = MessageQueue [String] deriving Show 

data Movement = North | South | East | West | NorthEast | NorthWest | SouthEast | SouthWest | None deriving Show

data PlayerData = PlayerData {playerName :: String, position :: Point, hitPoints :: Int} deriving (Show, Eq)

data MapData = MapData {level :: Int, grid :: Grid}

data GameState = GameState { mapData :: MapData, pcGen :: PCGen}

type Point = (Int, Int)

data TileType = Floor | Wall | UpStair | DownStair deriving (Show, Eq)

data Tile = Tile {tileGlyph :: Char, blocksMovement :: Bool, cellType :: TileType} deriving (Show, Eq)

data Entity = Entity {entityName :: String, entityGlyph :: Char, entityType :: EntityType, entityId :: Int} deriving Show

instance Eq Entity where
    x == y = entityId x == entityId y

data EntityType = Player | Monster | Item deriving (Show, Eq)

data Cell = Cell {entity :: Maybe Entity, tile :: Tile} deriving (Show, Eq)

data GridSize = GridSize {height :: Int, width :: Int} deriving (Show, Eq)

type Board = Array Point TileType

type Grid = Array Point Cell

type GridDelta = [(Point, Cell)]

type DeltaBoard = [(Point, TileType)]

data RenderMessage = RenderBoard DeltaBoard | GameOver deriving Show

data RenderState = RenderState {board :: Board, gameOver :: Bool} deriving Show

emptyMessageQueue :: MessageQueue
emptyMessageQueue = MessageQueue []

enqueueMessage :: String -> MessageQueue -> MessageQueue
enqueueMessage msg (MessageQueue msgs) = MessageQueue (msgs ++ [msg])

nextMessage :: MessageQueue -> (String, MessageQueue)
nextMessage (MessageQueue queue) = (head queue, MessageQueue (tail queue))