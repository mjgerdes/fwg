
module FWG.World where

import FWG.Biome
import FWG.Terrain
import FWG.Generate
import FWG.Random
import FWG.Tag
import FWG.Hex

type WorldMap = HexMap WorldHex

data WorldHex = WorldHex {
  _biome :: Biome,
  _terrain :: Terrain,
  _tags :: [Tag] }
  deriving (Show, Eq, Ord)
