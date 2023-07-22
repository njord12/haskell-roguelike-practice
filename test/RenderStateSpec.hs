{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Move brackets to avoid $" #-}
module RenderStateSpec
where

import SpecHelper
import RenderState (emptyBoard, buildInitialBoard, updateRenderState)
import Data.Array (Array, assocs)


spec :: Spec
spec =
    describe "Board manipulation" $ do
        context "create an empty grid" $
            it "should be only floors" $
                (assocs $ emptyBoard (BoardInfo 2 2)) `shouldMatchList` [((1,1) :: Point, Floor), ((1,2) :: Point, Floor), ((2,1) :: Point, Floor), ((2,2) :: Point, Floor)]
        context "initialize board with player position" $
            it "should have player at (1,2)" $
                (assocs $ board makeARenderState) `shouldMatchList` [((1,1) :: Point, Floor), ((1,2) :: Point, Player), ((2,1) :: Point, Floor), ((2,2) :: Point, Floor)]
        context "update board with board delta" $
            it "should return updated board" $
                (assocs $ board (updateRenderState  makeARenderState makeADeltaBoard)) `shouldMatchList` [((1,1) :: Point, Floor), ((1,2) :: Point, Floor), ((2,1) :: Point, Floor), ((2,2) :: Point, Player)]



main :: IO ()
main = hspec spec

makeARenderState :: RenderState
makeARenderState = buildInitialBoard (BoardInfo 2 2) (1,2)

makeADeltaBoard :: RenderMessage
makeADeltaBoard = RenderBoard [((1,2), Floor), ((2,2), Player)]