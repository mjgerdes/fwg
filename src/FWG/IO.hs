{-# LANGUAGE OverloadedStrings #-}
module FWG.IO where
import FWG.Terrain

import Data.Map (Map)
import qualified Data.Map as M
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.FilePath
import System.IO

import FWG.Hex

serializeTo :: (Show a) => a -> FilePath -> IO ()
serializeTo what filename = writeFile filename $ show what

unserializeFrom :: (Read a) => FilePath -> IO a
unserializeFrom filename = 
  do
    ws <- readFile filename
    return $ read ws

--loadTerrains :: IO [Terrain]
--loadTerrains = unserializeFrom "data/terrains"


--inspectMap :: (Ord k, Show k) => Map Text ((Map k a, k) -> (Text, k)) -> Map k a -> k -> IO ()
inspectMap :: Map Text ((Map Coords a, Coords) -> (Text, Coords)) -> Map Coords a -> Coords -> IO ()
inspectMap commands m coords = do
	   input <- TIO.getLine
	   let f = M.findWithDefault (\_ -> ("Unrecognized Command\n", coords)) input commands
	   let (w, newcoords@(x, y)) = f (m, coords)
	   putStrLn $ (show x) ++ ", " ++ (show y)
	   TIO.putStrLn w
	   inspectMap commands m newcoords


hexMapCommands :: Map Text ((HexMap Text, Coords) -> (Text, Coords))
hexMapCommands = M.fromList $ map f [("w", no), ("e", ne), ("d", se), ("s", so), ("a", sw), ("q", nw)]
  where f :: (Text, (Coords -> Coords)) -> (Text, (HexMap Text, Coords) -> (Text, Coords))
        f (w, move) = (w, \(m, xy) -> (M.findWithDefault "Nothing\n" (move xy) m, move xy))



