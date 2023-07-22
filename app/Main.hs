module Main (main) where
import System.IO (hReady, stdin)
import Types (Movement(..))

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