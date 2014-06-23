module Physics where

import Model
import Control.Conditional (cond)
import Data.Word


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

roundToNearestTile :: Float -> Int -> Float
roundToNearestTile x tileSize = fromIntegral $  floored - change
  where floored = floor (x + 25.0)
        change = floored `mod` tileSize

nextPos :: Player -> [[Char]] -> Int -> Point
nextPos player tiles dt = next
  where ps = pos player
        xspeed' = (xspeed player) * (fromIntegral dt) / 1000.0
        x' = (xpos ps) + (xspeed')
        y' = ypos ps
        next = if collidesX xelement (affectYRange y') tiles 
                then Point (roundToNearestTile (xpos ps) 50) y'
                else Point x' y'
        xelement = cond [(xspeed' > 0.0, last $ affectXRange x'),
                         (otherwise, head $ affectXRange x')]

jump :: Player -> Bool -> Player
jump p _ = p


applyGravity :: Player -> Int -> Player
applyGravity p _  = p
