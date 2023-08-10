module GameStateSpec
where

import SpecHelper
import Entities (makePlayer)
import Data.Array (assocs)
import Types (Movement(NorthEast))
import GameState (initializeMap)

spec :: Spec
spec =
    describe "GameState tests" $ do
        xit "moving north on the upper edge should not change position" $
           getAssocs (move getDefaultBoardInfo North makePlayer (initializeMap getDefaultBoardInfo)) `shouldMatchList` getAssocs (initializeMap getDefaultBoardInfo)
        xit "moving diagonally towards a border should not change position" $
            getAssocs (move getDefaultBoardInfo NorthEast makePlayer (initializeMap getDefaultBoardInfo)) `shouldMatchList` getAssocs (initializeMap getDefaultBoardInfo)


getDefaultBoardInfo :: GridSize
getDefaultBoardInfo = BoardInfo 2 2

getGridHelper :: GameState -> Grid
getGridHelper (GameState mapData _) = grid mapData

getAssocs :: GameState -> [(Point, Cell)]
getAssocs s = assocs $ getGridHelper s