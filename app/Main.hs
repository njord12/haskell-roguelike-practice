module Main (main) where

import RenderState (buildInitialBoard, render, BoardInfo (BoardInfo))

main :: IO ()
main = do
    let board = buildInitialBoard (BoardInfo 10 10) (3,3)
    putStr $ render (BoardInfo 10 10) board
