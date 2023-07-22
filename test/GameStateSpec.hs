module GameStateSpec
where

import SpecHelper
import GameState (moveHelper)


spec :: Spec
spec = 
    describe "GameState tests" $ do
        it "moving north on the upper edge should not change position" $
            moveHelper North (PlayerData (1,1) 0) getDefaultBoardInfo `shouldBe` (1,1)

        it "moving south on the lower edge should not change position" $
            moveHelper South (PlayerData (2,1) 0) getDefaultBoardInfo `shouldBe` (2,1)

        it "non-edge movement should change position" $
            moveHelper East (PlayerData (1,1) 0) getDefaultBoardInfo `shouldBe` (1,2)

        it "diagonal movement should work while moving opposite of corner" $
            moveHelper SouthEast (PlayerData (1,1) 0) getDefaultBoardInfo `shouldBe` (2,2)
        
        it "diagonal movement at edge corner should not change position" $
            moveHelper NorthWest (PlayerData (1,1) 0) getDefaultBoardInfo `shouldBe` (1,1)
        
        it "no movement should keep current position" $
            moveHelper None (PlayerData (1,1) 0) getDefaultBoardInfo `shouldBe` (1,1)


getDefaultBoardInfo :: BoardInfo
getDefaultBoardInfo = BoardInfo 2 2