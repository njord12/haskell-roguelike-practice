module Main (main) where
import System.IO (hReady, stdin, hSetBuffering, BufferMode (NoBuffering), hSetEcho, stdout, hSetBinaryMode)
import Types (Movement(..), GameState(..), BoardInfo (..), RenderState(..), PlayerData (PlayerData, position, hitPoints), MapData (..), Board)
import RenderState (buildInitialBoard, render)
import GameState (move)
import Data.Maybe (fromMaybe)

main :: IO ()
main = do
    let bInfo = BoardInfo 10 10
    let pData = PlayerData {position = (3,3), hitPoints = 10}
    let rState = buildInitialBoard bInfo (position pData)
    let grid = board rState
    let gState = GameState {player = pData, mapData = MapData 1 (10,10) grid}

    --These are needed so the key strokes are read without the need of pressing enter
    hSetBuffering stdin NoBuffering
    hSetEcho stdin False

    hSetBuffering stdout NoBuffering
    hSetBinaryMode stdout True
    
    gameLoop bInfo gState rState

gameLoop :: BoardInfo -> GameState -> RenderState -> IO ()
gameLoop binf state@(GameState player mapData) renderState = do
    putStr $ render binf (grid mapData) 
    key <- getKey
    let m = aux $ parseInput [head key]
    let gState' = move binf m state
    putStr "\ESC[2J" --This cleans the console screen
    gameLoop binf gState' renderState



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

aux :: Maybe Movement -> Movement
aux = Data.Maybe.fromMaybe None