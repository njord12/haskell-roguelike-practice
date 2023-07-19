{-# LANGUAGE BangPatterns #-}
module RenderState
where


import Data.Array (Array, listArray, (//))
import Data.Foldable (foldl')

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

cellToChar :: CellType -> String
cellToChar c =
    case c of 
        Floor -> ". "
        Player -> "@ "
        Wall -> "# "

render :: BoardInfo -> RenderState -> String
render binf@(BoardInfo h w) (RenderState b gOver) =
  if gOver
    then fst $ boardToString(emptyBoard binf)
    else fst $ boardToString b
  where 
    boardToString =  foldl' fprint ("", 0)
    fprint (!s, !i) cell = 
      if ((i + 1) `mod` w) == 0 
        then (s <> cellToChar cell <> "\n", i + 1 )
        else (s <> cellToChar cell , i + 1)