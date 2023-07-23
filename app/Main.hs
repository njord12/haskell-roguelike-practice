module Main (main) where
import System.IO (hReady, stdin, hSetBuffering, BufferMode (NoBuffering), hSetEcho, stdout, hSetBinaryMode)
import Types (Movement(..), GameState(..), BoardInfo (..), RenderState, PlayerData (PlayerData, position, hitPoints), MapData (..))
import RenderState (render, updateRenderState, buildInitialBoard)
import GameState (move)
import Data.Maybe (fromMaybe)

main :: IO ()
main = do
    let bInfo = BoardInfo 10 10
    let pData = PlayerData {position = (3,3), hitPoints = 10}
    let gState = GameState {player = pData, movement = None, mapData = MapData 1 (10,10)}
    let rState = buildInitialBoard bInfo (position pData)
    hSetBuffering stdin NoBuffering
    hSetEcho stdin False

    hSetBuffering stdout NoBuffering
    hSetBinaryMode stdout True
    gameLoop bInfo gState rState


--Todo:
--render place holder status line and messages window


--Gameloop:
--Render grid
--Read input
--update grid state
--clear display
--render grid again

gameLoop :: BoardInfo -> GameState -> RenderState -> IO ()
gameLoop binf state@(GameState player grid mov) renderState = do
    putStr $ render binf renderState
    key <- getKey
    let m = parseInput [head key]
    let (rMsg, gState') = move binf state{movement = aux m}
    let rState' = updateRenderState renderState rMsg
    putStr "\ESC[2J" --This cleans the console screen
    gameLoop binf gState' rState'



getKey :: IO [Char]
getKey = reverse <$> getKey' ""
 where
  getKey' chars = do
    char <- getChar
    more <- hReady stdin
    (if more then getKey' else return) (char : chars)


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