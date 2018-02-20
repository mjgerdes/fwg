
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE TypeFamilies              #-}

module FWG.Image where


import FWG.Generate

import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine
import Diagrams.TwoD.Text
import FWG.Biome

import qualified Data.Map as M

myCircle :: Diagram B
myCircle = circle 1


data HexImage = HexImage {
  imgHexCoords :: (Int, Int),
  imageBiome :: Biome,
  imgHexdanger :: Int,
  imgHexResources :: Int,
  imgHexNavigationDC :: Int,
  imgHexTerrainDC :: Int}
    deriving (Show, Eq, Ord)


colorFromBiome White = white
colorFromBiome Green = lightgreen
colorFromBiome DarkGreen = darkgreen
colorFromBiome Brown = brown
colorFromBiome Black = darkgrey
colorFromBiome Blue = blue
colorFromBiome Yellow = yellow
colorFromBiome Grey = lightgrey

infoString :: HexImage -> String
infoString (HexImage _ _ danger resources navDC _) = "D" ++ (show danger) ++ " R" ++ (show resources) ++ " N" ++ (show navDC)

coordString :: HexImage -> String
coordString (HexImage (x, y) _ _ _ _ _) = (show x) ++ ":" ++ (show y)


mkHex :: HexImage ->Diagram B
mkHex hex@(HexImage (x, y) biome danger resources navdc terraindc) = t2 <> (t1 <> (hexagon 1
  # lw medium
  # lc black
  # (fc $ colorFromBiome biome))) 
  where t1 = text (infoString hex) # fontSize (local 0.2) # fc black # moveOriginBy (r2 (0.0, 0.8))
        t2 = text (coordString hex) # fontSize (local 0.2) # fc black # moveOriginBy (r2 (0.0, -0.65))


apothem :: Double
apothem = (sqrt 3) / 2

gridPosition :: (Int, Int) -> P2 Double
gridPosition (x, y) = p2 (x', y')
  where x' = 1.5 * (fromIntegral x)
        y'
                  | odd x = -1.0 * apothem * (2.0 * (fromIntegral y) - 1.0)
                             | otherwise = -1.0 * 2.0 * apothem * (fromIntegral y)



makeHexImage :: ((Int, Int), Biome) -> HexStats -> HexImage
makeHexImage (coords, b) (HexStats danger res nav ter) = HexImage coords b danger res nav ter

hexGrid :: HexWorld -> Diagram B
hexGrid (bm, tm, sm) = position $ zip ps diagrams
    where ps = map (gridPosition.fst) $ M.toList bm
          diagrams = map (mkHex.(uncurry makeHexImage)) $ zip (M.toList bm) (map snd $ M.toList sm)


testhex = HexImage (0,0) (Green) 2 4 2 3
d1 = mkHex testhex

     --main = mainWith myCircle
