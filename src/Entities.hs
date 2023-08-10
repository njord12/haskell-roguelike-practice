module Entities(makePlayer, floorTile, wallTile)
where

import Types (Entity (..), EntityType (..), Tile (..), TileType (..))


makePlayer :: Entity
makePlayer = Entity "Player" '@' Player 1

floorTile :: Tile
floorTile = Tile '.' False Floor

wallTile :: Tile
wallTile = Tile '#' True Wall