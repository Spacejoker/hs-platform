module MapGenerator where

import System.Random
import Model

data Level = Level {
  lLayout :: [[Char]],
  lWidth :: Int,
  lHeight :: Int
}  

genMap :: Int -> Int -> Int -> IO(Level)
genMap w h numRooms = do
  rooms <- genRooms numRooms w h
  let level = Level (makeMap 0 rooms w h) w h
  let layout' = connectRooms level
  return (Level layout' w h)


connectRooms :: Level -> [[Char]]
connectRooms level@(Level layout w h) = layout
  where startCoord = findFreeCoord layout w h

findFreeCoord :: [[Char]] -> Int -> Int -> [Coord]
findFreeCoord layout w h = [(x, y) | x <- [0..(w-1)], y <- [0..(h-1)], valueAt (x, y) /= '#']

valueAt :: Coord -> [[Char]] -> Char
valueAt (x, y) l = (l !! y) !! x

genRooms :: Int -> Int -> Int -> IO( [Rect] )
genRooms 0 _ _ = return []
genRooms i w h= do
  w' <- getStdRandom (randomR (2, quot w 4))
  h' <- getStdRandom (randomR (2, quot h 4))
  x0 <- getStdRandom (randomR (0, w - w'))
  y0 <- getStdRandom (randomR (0, h - h'))
  nextRooms <- genRooms (i-1) w h
  return ((w',h',x0,y0):nextRooms)

makeMap :: Int -> [Rect] -> Int -> Int -> [[Char]]
makeMap y list mapWidth mapHeight
  | y == mapHeight = []
  | otherwise = ((makeMapRow 0 y list mapWidth mapHeight) : makeMap (y+1) list mapWidth mapHeight) 

makeMapRow :: Int -> Int -> [Rect] -> Int -> Int -> [Char]
makeMapRow x y list mapWidth mapHeight
  | x == mapWidth = []
  | isInRect x y mapWidth mapHeight list = "." ++ rest
  | otherwise = "#" ++ rest
    where rest = makeMapRow (x+1) y list mapWidth mapHeight

isInRect :: Int -> Int -> Int -> Int -> [Rect] -> Bool
isInRect _ _ _ _ [] = False
isInRect x y w h (z:zs) = pointInRect x y z w h || isInRect x y w h zs

-- check if a certain point is within the rect and also make sure that no edge is wall-less
pointInRect :: Int -> Int -> Rect -> Int -> Int -> Bool
pointInRect x y (a,b,x0,y0) w h = x >= x0 && y >= y0 && x < x0+a && y < y0 + b && x > 0 && y > 0 && x < w-1 && y < h-1

