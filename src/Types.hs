module Types
where
import Data.Array

newtype MessageQueue = MessageQueue [String] deriving Show 

data Movement = North | South | East | West | NorthEast | NorthWest | SouthEast | SouthWest | None deriving Show

data PlayerData = PlayerData {position :: Point, hitPoints :: Int} deriving (Show, Eq)

data MapData = MapData {level :: Int, stairsPosition :: Point}

data GameState = GameState {player :: PlayerData, mapData :: MapData, movement :: Movement}

type Point = (Int, Int)

data CellType = Floor | Player | Wall deriving (Show, Eq)

data BoardInfo = BoardInfo {height :: Int, width :: Int} deriving (Show, Eq)

type Board = Array Point CellType

type DeltaBoard = [(Point, CellType)]

data RenderMessage = RenderBoard DeltaBoard | GameOver deriving Show

data RenderState = RenderState {board :: Board, gameOver :: Bool} deriving Show

emptyMessageQueue :: MessageQueue
emptyMessageQueue = MessageQueue []

enqueueMessage :: String -> MessageQueue -> MessageQueue
enqueueMessage msg (MessageQueue msgs) = MessageQueue (msgs ++ [msg])

nextMessage :: MessageQueue -> (String, MessageQueue)
nextMessage (MessageQueue queue) = (head queue, MessageQueue (tail queue))