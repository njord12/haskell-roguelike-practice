{-# LANGUAGE BangPatterns #-}
module RenderState (updateRenderState, emptyBoard, render)
where


import Data.Array (listArray, (//))
import Data.Foldable (foldl')
import Types (RenderState(..), BoardInfo (..), Board, Point, RenderMessage(..), RenderState, TileType (..))


emptyBoard :: BoardInfo -> Board
emptyBoard (BoardInfo height width) = 
    listArray bounds emptyCells
    where
        bounds = ((1,1), (height, width))
        emptyCells = replicate (height * width) Floor


updateRenderState :: RenderState -> RenderMessage -> RenderState
updateRenderState (RenderState b gO) message =
    case message of
        RenderBoard delta ->
            RenderState (b // delta) gO
        GameOver ->
            RenderState b True

glyphToChar :: TileType -> String
glyphToChar c =
    case c of 
        Floor -> ". "
        Wall -> "# "
        UpStair -> "< "
        DownStair -> "> "

render :: BoardInfo -> Board -> String
render (BoardInfo _ w) board =
  fst $ boardToString board
  where
    boardToString = foldl' fprint ("", 0)
    fprint (!s, !i) cell =
      if((i + 1) `mod` w) == 0
        then (s <> glyphToChar cell <> "\n", i + 1)
        else (s <> glyphToChar cell, i + 1)
