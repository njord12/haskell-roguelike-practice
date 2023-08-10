{-# LANGUAGE BangPatterns #-}

module RenderState (updateRenderState, emptyBoard, render) where

import Data.Array (listArray, (//))
import Data.Foldable (foldl')
import Types (Board, Cell (Cell), Grid, GridSize (..), RenderMessage (..), RenderState (..), Tile (tileGlyph), TileType (..), entityGlyph)

emptyBoard :: GridSize -> Board
emptyBoard (BoardInfo height width) =
  listArray bounds emptyCells
  where
    bounds = ((1, 1), (height, width))
    emptyCells = replicate (height * width) Floor

updateRenderState :: RenderState -> RenderMessage -> RenderState
updateRenderState (RenderState b gO) message =
  case message of
    RenderBoard delta ->
      RenderState (b // delta) gO
    GameOver ->
      RenderState b True

render :: GridSize -> Grid -> String
render (BoardInfo _ w) g =
  fst $ gridToString g
  where
    gridToString = foldl' fprint ("", 0)
    fprint (!s, !i) cell =
      if ((i + 1) `mod` w) == 0
        then (s <> decideGlyph cell <> "\n", i + 1)
        else (s <> decideGlyph cell, i + 1)
    decideGlyph (Cell e t) =
      case e of
        Just entity -> entityGlyph entity : " "
        Nothing -> tileGlyph t : " "
