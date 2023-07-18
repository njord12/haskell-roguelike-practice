{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Move brackets to avoid $" #-}
module RenderStateSpec
where

import SpecHelper
import RenderState (emptyBoard, BoardInfo (BoardInfo))
import Data.Array (Array, assocs)


spec :: Spec
spec = 
    describe "Initialization" $ do
        context "empty grid" $
            it "should be only floors" $
                (assocs $ emptyBoard (BoardInfo 2 2)) `shouldMatchList` [((1,1) :: Point, Floor), ((1,2) :: Point, Floor), ((2,1) :: Point, Floor), ((2,2) :: Point, Floor)] 

main :: IO ()
main = hspec spec