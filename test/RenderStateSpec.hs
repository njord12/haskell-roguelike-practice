{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Move brackets to avoid $" #-}
module RenderStateSpec
where

import SpecHelper
import RenderState (emptyBoard, BoardInfo (BoardInfo), buildInitialBoard)
import Data.Array (Array, assocs)


spec :: Spec
spec = 
    describe "Initialization" $ do
        context "empty grid" $
            it "should be only floors" $
                (assocs $ emptyBoard (BoardInfo 2 2)) `shouldMatchList` [((1,1) :: Point, Floor), ((1,2) :: Point, Floor), ((2,1) :: Point, Floor), ((2,2) :: Point, Floor)] 
        context "initialize board" $
            it "should have player at (1,2)" $ 
                (assocs $ board (buildInitialBoard (BoardInfo 2 2) (1,2))) `shouldMatchList` [((1,1) :: Point, Floor), ((1,2) :: Point, Player), ((2,1) :: Point, Floor), ((2,2) :: Point, Floor)] 


main :: IO ()
main = hspec spec