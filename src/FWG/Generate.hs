{-# LANGUAGE OverloadedStrings #-}
module FWG.Generate where
import qualified Data.Traversable as TR
import FWG.Hex

import qualified Data.Set as S
import Data.List
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe
import FWG.Biome
import FWG.Probability
import FWG.Random
import FWG.Hex
import FWG.IO
import FWG.Landmark

generateTerrainMap :: (Coords -> Bool) -> ProbMap -> HexMap Text -> IO (HexMap Text)
generateTerrainMap boundsCheck p mm = rec (return mm) [(0, 0)]
  where rec :: IO (HexMap Text) -> [Coords] -> IO (HexMap Text)
        rec m [] = m
        rec m (xy:xys)
         | check xy =do
	 m' <- m
	 case M.lookup xy m' of
	  (Just _) -> rec m xys
	  (Nothing) -> do
	    terrain <- pick $ probsFromSurround xy p m'
	    rec (return $ M.insert xy (fromMaybe "FAIL" terrain) m') (xys ++ coordsAround xy)
	 	 
	 
           
--            | check xy = m >>= \m' -> (pick $ probsFromSurround xy p m') >>= \terrain -> if (isNothing $ M.lookup xy m') then if (isJust terrain) then (rec (return $ M.insert xy (fromJust terrain) m') (xys ++ (coordsAround xy))) else (rec (return m') (xys ++ [xy])) else rec (return m') xys
            | otherwise = rec m xys
        check = boundsCheck 
	
        

probsFromSurround :: Coords -> ProbMap -> HexMap Text -> [(Text, Double)]
probsFromSurround xy p m = map (\ (a, n) -> (a, average n)) $ M.toList $ foldl' f M.empty $ map (`M.lookup` m) $ coordsAround xy
  where f xs Nothing = xs
--        f xs (Just t) = xs ++ [(t, (ts >>= \t1 -> [M.findWithDefault 0.0 (t1, t) p]))]
        f accm (Just t) = foldl' g accm $ map (\x -> (x, M.findWithDefault 0.0 (t, x) p)) ts
--        g m Nothing = m
        g m (terrain, n) = M.insert terrain (n : (M.findWithDefault [] terrain m)) m
        ts = allProbKeys p



type BiomeDistribution = [(Biome, Double)]

defaultBiomeDistribution :: [(Biome, Double)]
defaultBiomeDistribution = zip a b
  where a = [White, Green, DarkGreen, Brown, Black, Blue]
  	b = [0.1, 0.3, 0.2, 0.2, 0.1, 0.1]

type Bounds = ((Int, Int), (Int, Int))

defaultBounds :: Bounds
defaultBounds = ((0,0), (19, 19))



type BiomePool = Map Biome [Biome]

makePool :: BiomeDistribution -> Bounds -> Map Biome [Biome]
makePool d ((x1,y1), (x2,y2)) = M.fromList $ map (g . f) d
  where (xsize, ysize) = (x2-x1+1, y2-y1+1)
  	f (b, p) = (b, floor $ p * (fromIntegral $ (xsize*ysize)))
	g (b, n) = (b, take n $ repeat b)


poolTake1 :: Biome -> Map Biome [Biome] -> (Maybe Biome, Map Biome [Biome])
poolTake1 b m = (listToMaybe xs, M.alter f b m)
  where xs = M.findWithDefault [] b m
  	f Nothing = Nothing
	f (Just xs) = Just $ drop 1 $ xs


seedBiomeMap :: [Biome] -> BiomePool -> Bounds -> IO (HexMap Biome, BiomePool)
seedBiomeMap inits d bounds = do 
	     bs <- sequence $ map f inits
	     return $ (M.fromList bs, foldr adjustPool d inits)
  where f b = do
  	  xy <- randomCoords bounds
	  return $ (xy, b)
        adjustPool b pool = snd $ poolTake1 b pool

growBiomeMap :: BiomePool -> Bounds -> HexMap Biome -> IO (HexMap Biome)
growBiomeMap pool bounds m' = rec (map fst $ M.toList m') pool m'
  where rec [] pool m = return m
        rec agenda pool m =
          do
            (x:xs) <- randomElement agenda
            case (freeHexesAround m bounds x) of
              [] -> rec xs pool m
              freeHexes -> 
                do
                  (free:_) <- randomElement freeHexes
                  let (Just thisb) = M.lookup x m
                  let (b, newpool) = poolTry1 thisb pool
                  rec (free:x:xs) newpool $ M.insert free b m




poolTry1 :: Biome -> BiomePool -> (Biome, BiomePool)
poolTry1 b pool = check $ poolTake1 b pool
  where check (Nothing, newpool) = f $ M.toList newpool
  	check (Just b, newpool) = (b, newpool)
	f [] = (Green, M.empty)
	f bs = g (head $ concat $ map snd bs, M.fromList bs)
	g (b, newpool) = let (Just b', pool') = poolTake1 b newpool in (b', pool')



filterBounds :: Bounds -> [Coords] -> [Coords]
filterBounds ((x1,y1),(x2,y2)) = filter f 
  where f (x, y) = and [x1 <= x, y1 <= y, x <= x2, y <= y2]

freeHexesAround :: HexMap a -> Bounds -> Coords -> [Coords]
freeHexesAround m bounds coords = foldl' (\xs -> \c -> f xs $ (M.lookup c m, c)) [] $ filterBounds bounds $ coordsAround coords
  where f xs (Nothing, x) = x:xs
  	f xs ((Just _), x) = xs



defaultBiomes = [White, Green, Green, DarkGreen, Brown, Blue, Black]

mkb = generateBiomeMap defaultBiomeDistribution defaultBounds

generateBiomeMap :: BiomeDistribution -> Bounds -> IO (HexMap Biome)
generateBiomeMap d bounds = do
		 (s, newpool) <- seedBiomeMap defaultBiomes pool bounds
		 growBiomeMap newpool bounds s
  where pool = makePool d bounds


generateBiomeMapFromSeed :: BiomeDistribution -> Bounds -> HexMap Biome -> IO (HexMap Biome)
generateBiomeMapFromSeed d bounds s = do
		 growBiomeMap pool bounds s
  where pool = foldl subtractGiven (makePool d bounds) $ fmap snd $ M.toList s
        subtractGiven pool biome = snd $ poolTake1 biome pool

meets :: (Eq a, Ord a) => HexMap a -> Coords -> Int
meets m xy = (S.size $ S.fromList $ map fromJust $ filter isJust $ [M.lookup xy m] ++ (map (`M.lookup` m) $ coordsAround xy)) - 1

countInHexMap :: (Eq a) => a -> HexMap a -> Int
countInHexMap x m = length $ filter (==x) $ map snd $ M.toList m

{-#
meets m xy = try $ M.lookup xy m
  where try Nothing = 0
  	try (Just gold) = foldl' (f gold) 0 $ map (`M.lookup` m) $ coordsAround xy
	f gold n Nothing = n
	f gold n (Just x) 
	  | gold == x = n
	  | otherwise = n+1
#-}

allMeets :: (Eq a, Ord a) => HexMap a -> [(Coords, Int)]
allMeets m = fmapCoords (meets m) m




data HexType = Center Biome | Edge Biome | Meet Biome


hextypes :: HexMap Biome -> HexMap HexType
hextypes m = M.fromList $ map f $ allMeets m
  where threshold = 2
  	biome = M.findWithDefault (Green) 
  	f (xy, n) 
	  | n >= threshold = (xy, Meet $ biome xy m)
	  | n == 1 = (xy, Edge $ biome xy m)
	  | otherwise = (xy, Center $ biome xy m)


makeMap :: IO(HexMap Biome, HexMap Text)
makeMap = do
	biomes <- generateBiomeMap defaultBiomeDistribution defaultBounds
	let hexes = hextypes biomes
	m <- populateWithTerrain hexes
	return (biomes, m)

mergeMaps biomes terrains = M.fromList $ zipWith f (M.toList biomes) (M.toList terrains)
  where f (coords, biome) (_, terrain) = (coords, T.pack $ (show biome) ++ ", " ++ (T.unpack terrain))


populateWithTerrain :: HexMap HexType -> IO (HexMap Text)
populateWithTerrain m = do
  centers <- getProbMapFromFile "data/center"
  edges <- getProbMapFromFile "data/edge"
  meets <- getProbMapFromFile "data/meet"
  TR.sequence $ fmap (decideTerrain (centers, edges, meets)) m

decideTerrain :: (ProbMap, ProbMap, ProbMap) -> HexType -> IO Text
decideTerrain (centers, edges, meets) hex = f hex >>= return . fromMaybe ""
  where f (Center b) = pick $ distributionFor (btext b) centers
  	f (Edge b) = pick $ distributionFor (btext b) edges
	f (Meet b) = pick $ distributionFor (btext b) meets
	btext b = T.pack $ show b



		    


putMountains :: Int -> HexMap Text -> HexMap Text
putMountains threshold m = foldl' f m $ filter (\ (_, n) -> n >= threshold) $ allMeets m
  where f m (xy, _) = M.alter (const (Just "mountains")) xy m


mkc = do
  m <- mkb
  return $ fmap (T.pack . show) m

fmapCoords :: (Coords -> a) -> HexMap b -> [(Coords, a)]
fmapCoords f = map g . M.keys
  where g x = (x, f x)

mk = makeTerrainProbMap
ts = map T.pack $ ["plains","forest","mountains","hills"]
testm :: HexMap Text
testm =  f (1,0) "plains" $ f (0,1) "forest" $ f (0, -1) "plains" M.empty
  where f = M.insert

testbounds (x,y) = x >= 0 && x < 10 && y >= 0 && y < 8

borderLandsDistribution = [(White, 0.12), (Green, 0.255), (DarkGreen, 0.125), (Brown, 0.20), (Black, 0.09), (Yellow, 0.05), (Grey, 0.15), (Blue, 0.01)]
borderLandsSeed = M.fromList $ [((0, y), Grey) | y <- [0 .. 7]] ++ [((x, 0), Grey) | x <- [0 .. 10]] ++
  [((5, 6), Blue)] ++
  [((5, 7), Grey), ((12, 7), Grey), ((12,8), Grey), ((11,8), Grey)] ++
  [((x, y), Green) | x <- [1 .. 5], y <- [1 .. 4]] ++
  [(coords, Brown) | coords <- [(4,7), (5, 8), (6,7), (6, 8), (6,6)]] ++
    [((x, y), DarkGreen) | x <- [0 .. 3], y <- [9 .. 11]] ++
    [((x, y), White) | x <- [12 .. 14], y <- [0 .. 4]] ++
    [((x, y), Black) | x <- [12 .. 14], y <- [9 .. 11]] ++
    [(coords, Yellow) | coords <- [(8, 6), (9, 6), (9, 7)]] ++
      [(coords, Green) | coords <- [(8, 11), (7, 11), (7, 10)]] ++
        [(coords, Blue) | coords <- [(14, 6), (13, 6), (13, 7)]]
    

borderLandsBounds = ((0,0), (14, 11))
makeBorderlands = do
  bs <- generateBiomeMapFromSeed borderLandsDistribution borderLandsBounds borderLandsSeed
  terrain <- populateWithTerrain $ hextypes bs
  return $ (fmap (T.pack.show) bs, terrain)


makeBorderlands' = do
  bs <- generateBiomeMapFromSeed borderLandsDistribution borderLandsBounds borderLandsSeed
  terrain <- populateWithTerrain $ hextypes bs
  stats <- makeStats bs
  f <- landmarksFunc
  terrain' <- giveLandmarks f bs terrain
  return $ (bs, terrain', stats)

giveLandmarks :: (Biome -> [Text]) -> HexMap Biome -> HexMap Text -> IO (HexMap Text)
giveLandmarks f bm tm = do
  ls <- sequence $ map (randomElement.f) bs
  return $ M.fromList $ zipWith g ts $ map head ls
  where (coords, bs) = unzip $ M.toList bm
        ts = M.toList tm
        g (coords, terrain) landmark = (coords, T.append (T.append terrain ", ") landmark)

makeStats :: HexMap Biome -> IO (HexMap HexStats)
makeStats bm = do
  stats <- sequence $ map hexStatsFromBiome bs
  return $ M.fromList $ zip coords stats
    where (coords, bs) = unzip $ M.toList bm



type HexWorld = (HexMap Biome, HexMap Text, HexMap HexStats)

data HexStats = HexStats {
  hexDanger :: Int,
  hexResources :: Int,
  hexNavDC :: Int,
  hexTerrainDC :: Int}
  deriving (Show, Eq, Ord)


defaultHexStats = HexStats 0 0 0 0

hexStatsFromBiome b = f b where
  f Green = do
    danger <- rollD 1 2
    res <- roll 1 2
    nav <- roll 1 2
    ter <- return 2
    return $ HexStats danger (res + 2) nav ter
  f Brown = do
    danger <- roll 1 2
    res <- roll 1 2
    nav <- rollA 1 2
    ter <- return 2
    return $ HexStats danger (res + 1) nav ter
  f DarkGreen = do
    danger <- rollD 1 3
    res <- roll 1 3
    nav <- roll 1 2
    ter <- return 3
    return $ HexStats danger (res + 1) (nav + 2) ter
  f White = do
    danger <- rollA 1 2
    res <- roll 1 2
    nav <- rollA 1 2
    ter <- return 4
    return $ HexStats danger (res - 1) (nav +1) ter
  f Black = do
    danger <- rollA 1 2
    res <- return 0
    nav <- roll 1 3
    ter <- return 3
    return $ HexStats (danger + 2) res (nav + 2) ter
  f Yellow = do
    danger <- rollD 1 2
    res <- rollD 1 2
    nav <- return 4
    ter <- return 3
    return $ HexStats danger (res - 1) nav ter
  f Grey = do
    danger <- roll 1 2
    res <- roll 1 2
    nav <- rollD 1 2
    ter <- return 4
    return $ HexStats danger res (nav + 1) ter
  f Blue = do
    danger <- rollD 1 2
    res <- roll 1 3
    nav <- return 4
    ter <- return 4
    return $ HexStats danger res nav ter


insp m = inspectMap hexMapCommands m (0,0)



type WorldState = (Coords, HexWorld)

infoText :: Coords -> HexMap HexStats -> Text
infoText coords sm = T.pack $ "\nD" ++ d ++ "\nR" ++ r ++ "\nN" ++ n ++ "\nT" ++ t 
  where stats = M.findWithDefault defaultHexStats coords sm
        (d, r, n, t) = (show $ hexDanger stats, show $ hexResources stats, show $ hexNavDC stats, show $ hexTerrainDC stats)

eventTimes = words "Morning Afternoon Evening Dusk Midnight Predawn"

rollEvents :: Int -> IO [String]
rollEvents danger = do
  dice <- sequence $ take 6 $ repeat $ roll 1 6
  return $ map snd $ filter (\(n, _) -> n <= danger) $ zip dice eventTimes

dayString :: HexStats -> IO String
dayString (HexStats danger _ _ terrain) = do
  events <- rollEvents danger
  n1 <- roll 1 6
  n2 <- roll 1 6
  return $ (unlines events) ++ (progress terrain n1) ++ discovery n2
  where progress dc roll = "\nProgress: " ++ (show roll) ++ if roll > dc then ", Success!" else "Failure!"
        discovery roll = if roll == 1 then "Discovery!" else ""

worldCommands :: Text -> WorldState -> IO (Text, WorldState)
worldCommands cmd = M.findWithDefault (\state -> return $ ("Unrecognized command", state)) cmd cmds where
  cmds :: Map Text (WorldState -> (IO (Text, WorldState)))
  cmds = M.fromList $ (map (\(c, f) -> (c, (\(coords, world@(bs, _, _)) -> return $ (T.pack $ show $ M.findWithDefault Blue (f coords) bs, (f coords, world))))) directions) ++ morecmds
  morecmds = [("l", \state@(coords, (_, tm, _)) -> return (M.findWithDefault T.empty coords tm, state)),
    ("ls", \state@(coords, (_, _, sm)) -> return (infoText coords sm, state)),
    ("r", \state@(coords, (_, _, sm)) -> (dayString $ M.findWithDefault defaultHexStats coords sm) >>= \w -> return (T.pack w, state))]
  directions = [("w", no), ("e", ne), ("d", se), ("s", so), ("a", sw), ("q", nw)]

inspectWorld :: HexWorld -> IO ()
inspectWorld world = inspectWorld' world (0,0)
  where inspectWorld' (bm, tm, sm) coords = do {
    input <- TIO.getLine;
    (msg, (newCoords@(x, y), newWorld)) <- worldCommands input (coords, (bm, tm, sm));
    putStrLn $ (show x) ++ ", " ++ (show y);
    TIO.putStrLn msg;
    inspectWorld' newWorld newCoords}

