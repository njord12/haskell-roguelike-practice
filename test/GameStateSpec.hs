module GameStateSpec
where

import SpecHelper
import Entities (makePlayer)
import Data.Array (assocs)

spec :: Spec
spec =
    describe "GameState tests" $ do
        it "moving north on the upper edge should not change position" $
           getAssocs (move getDefaultBoardInfo North makePlayer (initializeMap getDefaultBoardInfo)) `shouldMatchList` getAssocs (initializeMap getDefaultBoardInfo)


getDefaultBoardInfo :: GridSize
getDefaultBoardInfo = BoardInfo 2 2

getGridHelper :: GameState -> Grid
getGridHelper (GameState mapData) = grid mapData

getAssocs :: GameState -> [(Point, Cell)]
getAssocs s = assocs $ getGridHelper s