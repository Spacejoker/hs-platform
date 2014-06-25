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

isTileColliding :: [(Int, Int)] -> [[Char]] -> Bool
isTileColliding [] _ = False
isTileColliding (x:xs) m = (collideTest x m) || isTileColliding xs m

collideTest :: (Int, Int) -> [[Char]] -> Bool
collideTest (x, y) tiles = ((tiles !! y) !! x) == '1'

roundToNearestTile :: Float -> Int -> Float
roundToNearestTile x tileSize = fromIntegral $  floored - change
  where floored = floor (x + 25.0)
        change = floored `mod` tileSize

getDist :: Float -> Int -> Float
getDist speed dt = speed * (fromIntegral dt) / 1000.0

nextXPos :: Player -> [[Char]] -> Int -> Float
nextXPos player tiles dt = finalx
  where ps = pos player
        xdist = getDist (xspeed player) dt
        ydist = getDist (yspeed player) dt
        oldx = xpos ps
        oldy = ypos ps
        x' = oldx + (xdist)
        y' = oldy + (ydist)
        xelement
          | xdist > 0.0 = last $ affectXRange x'
          | otherwise = head $ affectXRange x'
        xCheck = [(xelement, b) | b <- (affectYRange y')]
        finalx
          | isTileColliding xCheck tiles = (roundToNearestTile (xpos ps) 50)
          | otherwise = x'

nextPos :: Player -> [[Char]] -> Int -> Point
nextPos player tiles dt = Point (nextXPos player tiles dt) finaly
  where ps = pos player
        xdist = getDist (xspeed player) dt
        ydist = getDist (yspeed player) dt
        oldx = xpos ps
        oldy = ypos ps
        x' = oldx + (xdist)
        y' = oldy + (ydist)
        xelement
          | xdist > 0.0 = last $ affectXRange x'
          | otherwise = head $ affectXRange x'
        yelement 
          | ydist > 0.0 = last $ affectYRange y'
          | otherwise = head $ affectYRange y'
        xCheck = [(xelement, b) | b <- (affectYRange y')]
        yCheck = [(a, yelement) | a <- (affectXRange x')]
        finalx = if isTileColliding xCheck tiles 
                  then (roundToNearestTile (xpos ps) 50)
                  else x'
        finaly 
          | isTileColliding yCheck tiles = (roundToNearestTile (xpos ps) 50)
          | otherwise = y'

isPlayerOnGround a b = True

jump :: Player -> [[Char]] -> Player
jump p m
  | isPlayerOnGround p m = p { yspeed = -10 }
  | otherwise = p


applyGravity :: Player -> Int -> Player
applyGravity p dt  = p { yspeed = (yspeed p) + ((fromIntegral dt) * (gravity p)) }
  where oldSpeed = yspeed p
        acceleration = (fromIntegral dt) * (gravity p)

pixelWall :: Point -> [[Char]] -> Bool
pixelWall p tiles = ((tiles !! y') !! x') == '1'
  where x = ((floor(xpos p))::Int)
        y = ((floor(ypos p))::Int)
        x' = x `quot` 50
        y' = y `quot` 50
