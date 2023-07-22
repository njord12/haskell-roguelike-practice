module Main (main) where

import RenderState (render, BoardInfo (BoardInfo))

main :: IO ()
main = do
    putStrLn "nothing"


--Todo:
--render place holder status line and messages window


--Gameloop:
--Render grid
--Read input
--update grid state
--clear display
--render grid again