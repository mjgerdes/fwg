
module FWG.Random where


import FWG.Hex
import Data.List
import System.Random

pick :: [(a, Double)] -> IO (Maybe a)
pick xs = 
  do
    n <- getStdRandom (randomR (0.0, 1.0))
    return $ pickBy n xs

directions :: [Coords -> Coords]
directions = [no, ne, se, so, sw, nw]

randomDirection :: IO (Coords -> Coords)
randomDirection = do
		n <- getStdRandom $ randomR (0, ((length directions) - 1))
		return $ directions !! n

randomElement :: [a] -> IO [a]
randomElement xs = do
	      n <- getStdRandom $ randomR (0, length xs -1)
	      let (as, bs) = splitAt n xs
	      return $ bs ++ as

randomCoords :: ((Int, Int), (Int, Int)) -> IO Coords
randomCoords ((x1,y1),(x2,y2)) = do
	    x <- getStdRandom $ randomR(x1, x2)
	    y <- getStdRandom $ randomR (y1,y2)
	    return (x,y)

pickBy :: Double -> [(a, Double)] -> Maybe a
pickBy n xs = fst $ foldl' f (Nothing, 0.0) xs
  where f pair@(Just x, _) _ = pair
        f (Nothing, acc) (x, p)
            | (acc <= n) && (n <= acc+p) = (Just x, acc+p)
            | otherwise = (Nothing, acc+p)

roll :: Int -> Int -> IO Int
roll _ 0 = return 0
roll n sides = do
  dice <- sequence $ map randomElement $ take n $ repeat [1 .. sides]
  return $ sum $ map head dice


rollF :: (Int -> Int -> Int) -> Int -> Int -> IO Int
rollF f n sides = do
  a <- roll n sides
  b <- roll n sides
  return $ f a b


rollD = rollF min
rollA = rollF max
