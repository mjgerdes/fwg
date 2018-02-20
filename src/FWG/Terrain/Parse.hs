

module FWG.Terrain.Parse where


import FWG.Terrain

import Text.Parsec
import Data.Text (Text)
import qualified Data.Text as T


tx = T.pack

loadTerrain :: IO (Either ParseError [Terrain])
loadTerrain = readFile fn >>= return . parse terrains fn
  where fn = "data/terrain"
terrains :: Parsec String () [Terrain]
terrains = many terrainBlock

terrainBlock = do
	     name <- line
	     displayname <- line
	     description <- line
	     features <- many line
	     newline
	     return $ Terrain (tx name) (tx displayname) (tx description) (fmap tx features)

line = do
     w <- many1 $ noneOf "\n"
     newline
     return w
