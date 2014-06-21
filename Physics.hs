module Physics where

import Model
import Control.Conditional (cond)

-- get affected y-layers
affectYRange :: Float -> [Int]
affectYRange y = [a..b]
  where a = floor (y/50.0 + 0.000001)
        b = floor ((y+100.0)/50 - 0.000001)

affectXRange :: Float -> [Int]
affectXRange x = [a..b]
  where a = floor (x/50.0 + 0.000001)
        b = floor ((x+50.0)/50 - 0.000001)


collidesX :: Int -> [Int] -> [[Char]] -> Bool
collidesX _ [] _ = False
collidesX x (y:ys) tiles
  | ((tiles !! y) !! x) == '1' = True
  | otherwise = collidesX x ys tiles

nextPos :: Player -> [[Char]] -> Point
nextPos player tiles = next
  where ps = pos player
        x' = (xpos ps) + (xspeed player)
        y' = ypos ps
        next = if collidesX xelement (affectYRange y') tiles 
                then Point (xpos ps) y'
                else Point x' y'
        xelement = cond [((xspeed player) > 0.0, last $ affectXRange x'),
                         (otherwise, head $ affectXRange x')]

