{-# LANGUAGE OverloadedStrings #-}
module FWG.Probability where

import Data.List
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Map (Map)
import qualified Data.Map as M
import qualified Data.Set as S

type Bigram = (Text, Text)
type ProbMap = Map Bigram Double

readProbPair :: [Text] -> (Text, Double)
readProbPair [w, n] = (w, read $ T.unpack n)

preprocessProbText :: Text -> [((Text, Text), Double)]
preprocessProbText w = concat $ map (f . T.lines) $ T.splitOn "\n\n" w
  where f (w1:ws) = ws >>= \w -> let [w2, n] = T.words w in return $ ((w1, w2), read $ T.unpack n)
        f _ = []

readProb :: Text -> [Text] -> ProbMap -> ProbMap
readProb w1 ws m = M.insert (w1, w2) n m
  where (w2, n) = readProbPair ws


getProbMapFromFile :: FilePath -> IO ProbMap
getProbMapFromFile filename = do
		   w <- TIO.readFile filename
		   return $ M.fromList $ preprocessProbText w



distributionFromFile :: Text -> FilePath -> IO [(Text, Double)]
distributionFromFile w filename = getProbMapFromFile filename >>= return . distributionFor w

distributionFor :: Text -> ProbMap -> [(Text, Double)]
distributionFor w p = map (\ ((_, w2), n) -> (w2, n)) $ filter f $ M.toList p
  where f ((w1, _), _) = w == w1


makeTerrainProbMap :: IO (ProbMap)
makeTerrainProbMap =
  do
    w <- TIO.readFile "data/terrain_probabilities"
    return $ M.fromList $ preprocessProbText w



average :: [Double] -> Double
average xs = (foldl' (+) 0 xs ) / (fromIntegral $ length xs)


allProbKeys :: ProbMap -> [Text]
allProbKeys = S.toList . S.fromList . map snd . M.keys


checkProbMap :: ProbMap -> [(Text, Double)]
checkProbMap = filter ((/=1.0) . snd) . checkProbMap'

checkProbMap' :: ProbMap -> [(Text, Double)]
checkProbMap' p = map g $ map f $ ts
  where ts = allProbKeys p
        f t1 = (t1, map (\x -> M.findWithDefault 0.0 (t1, x) p) ts)
        g (w, ns) = (w, sum ns)
