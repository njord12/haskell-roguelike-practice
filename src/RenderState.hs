{-# LANGUAGE BangPatterns #-}
module RenderState (render, updateRenderState, buildInitialBoard, emptyBoard, renderNew)
where


import Data.Array (listArray, (//))
import Data.Foldable (foldl')
import Types (RenderState(..), BoardInfo (..), Board, Point, RenderMessage(..), RenderState, CellType (..))


emptyBoard :: BoardInfo -> Board
emptyBoard (BoardInfo height width) = 
    listArray bounds emptyCells
    where
        bounds = ((1,1), (height, width))
        emptyCells = replicate (height * width) Floor

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

glyphToChar :: CellType -> String
glyphToChar c =
    case c of 
        Floor -> ". "
        Player -> "@ "
        Wall -> "# "

render :: BoardInfo -> RenderState -> String
render binf@(BoardInfo _ w) (RenderState b gOver) =
  if gOver
    then fst $ boardToString(emptyBoard binf)
    else fst $ boardToString b
  where 
    boardToString =  foldl' fprint ("", 0)
    fprint (!s, !i) cell = 
      if ((i + 1) `mod` w) == 0 
        then (s <> glyphToChar cell <> "\n", i + 1 )
        else (s <> glyphToChar cell , i + 1)


renderNew :: BoardInfo -> Board -> String
renderNew (BoardInfo _ w) board =
  fst $ boardToString board
  where
    boardToString = foldl' fprint ("", 0)
    fprint (!s, !i) cell =
      if((i + 1) `mod` w) == 0
        then (s <> glyphToChar cell <> "\n", i + 1)
        else (s <> glyphToChar cell, i + 1)
