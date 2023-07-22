module GameStateSpec
where

import SpecHelper
import GameState (moveHelper)
import Types 
import RenderState (BoardInfo(BoardInfo))


spec :: Spec
spec = 
    describe "GameState tests" $ do
        it "moving north on the upper edge should not change position" $
            moveHelper North (PlayerData (1,1) 0) (BoardInfo 2 2) `shouldBe` (1,1)

        it "moving south on the lower edge should not change position" $
            moveHelper South (PlayerData (2,1) 0) (BoardInfo 2 2) `shouldBe` (2,1)

        it "non-edge movement should change position" $
            moveHelper East (PlayerData (1,1) 0) (BoardInfo 2 2) `shouldBe` (1,2)

        it "diagonal movement should work while not moving past corner" $
            moveHelper SouthEast (PlayerData (1,1) 0) (BoardInfo 2 2) `shouldBe` (2,2)