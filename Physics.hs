module Physics where

import Model

-- get affected y-layers
--getYRange :: Int -> [Int]
--getYRange y =

nextPos :: Player -> [[Char]] -> Point
nextPos player tiles = Point x' y'
  where ps = pos player
        x' = (xpos ps) + (xspeed player)
        y' = ypos ps

