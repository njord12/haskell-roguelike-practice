module RenderState
where


import Data.Array (Array, listArray, (//))

type Point = (Int, Int)

data CellType = Floor | Player | Wall deriving (Show, Eq)

data BoardInfo = BoardInfo {height :: Int, width :: Int} deriving (Show, Eq)
type Board = Array Point CellType

type DeltaBoard = [(Point, CellType)]

data RenderMessage = RenderBoard DeltaBoard | GameOver deriving Show

data RenderState = RenderState {board :: Board, gameOver :: Bool} deriving Show

emptyBoard :: BoardInfo -> Board
emptyBoard (BoardInfo height width) = 
    listArray bounds emptyCells
    where
        bounds = ((1,1), (height, width))
        emptyCells = replicate (height * width) Floor

-- >>> emptyBoard (BoardInfo 2 2)

buildInitialBoard :: BoardInfo -> Point -> RenderState
buildInitialBoard bi iniPlayer =
    RenderState (emptyBoard  bi // [(iniPlayer, Player)]) False


updateRenderState :: RenderState -> RenderMessage -> RenderState
updateRenderState (RenderState b gO) message =
    case message of
        RenderBoard delta ->
            RenderState (b // delta) gO
        GameOver ->
            RenderState b True