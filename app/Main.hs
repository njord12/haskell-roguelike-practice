module Main (main) where
import System.IO (hReady, stdin, hSetBuffering, BufferMode (NoBuffering), hSetEcho, stdout, hSetBinaryMode)
import Types (Movement(..), GameState(..), GridSize (..),  MapData (..))
import RenderState (render)
import GameState (move, initializeMap)
import Data.Maybe (fromMaybe)
import Entities (makePlayer)

main :: IO ()
main = do
    let bInfo = GridSize 20 20
    let gState = initializeMap bInfo

    --These are needed so the key strokes are read without the need of pressing enter
    hSetBuffering stdin NoBuffering
    hSetEcho stdin False

    hSetBuffering stdout NoBuffering
    hSetBinaryMode stdout True
    
    gameLoop bInfo gState

gameLoop :: GridSize -> GameState -> IO ()
gameLoop binf state@(GameState mapData gen) = do
    putStr $ render binf (grid mapData) 
    key <- getKey
    let m = fromMaybe None (parseInput [head key])
    let gState' = move binf m makePlayer state
    putStr "\ESC[2J" --This cleans the console screen
    gameLoop binf gState'



getKey :: IO [Char]
getKey = reverse <$> getKey' ""
 where
  getKey' chars = do
    char <- getChar
    more <- hReady stdin
    (if more then getKey' else return) (char : chars)

--having this as string for implementing some sort of buffered input in the future
parseInput :: String -> Maybe Movement
parseInput "k" = Just North
parseInput "j" = Just South
parseInput "l" = Just East
parseInput "h" = Just West
parseInput "u" = Just NorthEast
parseInput "y" = Just NorthWest
parseInput "n" = Just SouthEast
parseInput "b" = Just SouthWest
parseInput _ = Nothing