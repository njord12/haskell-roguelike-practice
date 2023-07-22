module Types
where


newtype MessageQueue = MessageQueue [String] deriving Show 

data Movement = North | South | East | West | NorthEast | NorthWest | SouthEast | SouthWest deriving Show

data Player = Player {postition :: Point, hitPoints :: Int} deriving (Show, Eq)

data MapData = MapData {level :: Int, stairsPosition :: Point}

data GameState = GameState {player :: Player, mapData :: MapData, movement :: Movement}

type Point = (Int, Int)

emptyMessageQueue :: MessageQueue
emptyMessageQueue = MessageQueue []

enqueueMessage :: String -> MessageQueue -> MessageQueue
enqueueMessage msg (MessageQueue msgs) = MessageQueue (msgs ++ [msg])

nextMessage :: MessageQueue -> (String, MessageQueue)
nextMessage (MessageQueue queue) = (head queue, MessageQueue (tail queue))