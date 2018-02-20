{-# LANGUAGE TemplateHaskell #-}
module FWG.Terrain where


import Data.Text (Text)
import qualified Data.Text as T
import Control.Lens




data Terrain = Terrain {
  _terrainName :: Text,
  _terrainDisplayName :: Text,
  _terrainDescription :: Text,
  _terrainFeatures :: [Text] }
  deriving (Show, Eq, Ord)





