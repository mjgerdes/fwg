{-# LANGUAGE OverloadedStrings #-}
module FWG.Landmark where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Map (Map)
import qualified Data.Map as M
import qualified Data.Set as S
import FWG.Biome


getLandmarksFromFile :: FilePath -> IO (Biome -> [Text])
getLandmarksFromFile fp = do
  w <-     TIO.readFile fp
  return $ (\key -> M.findWithDefault [T.empty] key $ M.fromList $ preprocessLandmarkFile w)


preprocessLandmarkFile :: Text -> [(Biome, [Text])]
preprocessLandmarkFile w = map (f.T.lines) $ T.splitOn "\n\n" w
  where f (w:ws) = ((read $ T.unpack w) :: Biome, ws)

landmarksFunc = getLandmarksFromFile "data/landmarks" 





