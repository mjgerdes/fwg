
module FWG.Hex where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Map (Map)
import qualified Data.Map as M


type Coords = (Int, Int)
type HexMap a = Map Coords a
no :: Coords -> Coords
no (x, y) = (x, y-1)

so :: Coords -> Coords
so (x, y) = (x, y+1)

ne :: Coords -> Coords
ne (x, y)
        | mod x 2 == 0 = (x+1, y)
        | otherwise = (x+1, y-1)

se :: Coords -> Coords
se (x, y)
        | mod x 2 == 0 = (x+1, y+1)
        | otherwise = (x+1, y)

nw :: Coords -> Coords
nw (x, y) 
        | mod x 2 == 0 = (x-1, y)
        | otherwise = (x-1, y-1)

sw :: Coords -> Coords
sw (x, y)
        | mod x 2 == 0 = (x-1, y+1)
        | otherwise = (x-1, y)


coordsAround :: Coords -> [Coords]
coordsAround xy = map ($ xy) [no, ne, se, so, sw, nw]
