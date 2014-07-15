module MapGenerator where

import System.Random
import Model
import Data.List


genMap :: Int -> Int -> Int -> IO(Level)
genMap w h numRooms = do
  rooms <- genRooms numRooms w h
  let level = makeMap rooms
  let layout' = connectRooms ( rmdups level ) 10
  -- let layout' = rmdups level
  return (Level layout' w h)

rmdups :: (Ord a) => [a] -> [a]
rmdups = map head . group . sort

connectRooms :: [MapCoord] -> Int -> [MapCoord]
connectRooms layout cnt
  | length nonReach > 0 && cnt > 0 = connectRooms ( layout ++ added) (cnt-1)
  | otherwise = reach
  where reach = dfs layout [(head layout)] []
        nonReach = filter (\x -> not $ elem x reach) layout
        added = makePath (head reach) (head nonReach)

makePath :: MapCoord -> MapCoord -> [MapCoord]
makePath (x0, y0, _) (x1, y1, _) = [(x, y, '.') | x <- [x0 .. x1], y <- [y0 .. y1], x == x1 || y == y0]

dfs :: [MapCoord] -> [MapCoord] -> [MapCoord] -> [MapCoord]
dfs _ [] visited = visited
dfs searchSpace ((x, y, z):xs) visited 
  | elem (x, y, z) visited || (not $ elem (x, y, z) searchSpace) = dfs searchSpace xs visited
  | otherwise = dfs searchSpace (cands ++ xs) ((x, y, z) : visited) 
            where cands = [(x+1,y, z), (x-1, y, z), (x, y+1, z), (x, y-1, z)]

genRooms :: Int -> Int -> Int -> IO( [Rect] )
genRooms 0 _ _ = return []
genRooms i w h= do
  w' <- getStdRandom (randomR (2, 4))
  h' <- getStdRandom (randomR (2, 4))
  x0 <- getStdRandom (randomR (0, w - w'))
  y0 <- getStdRandom (randomR (0, h - h'))
  nextRooms <- genRooms (i-1) w h
  return ((x0, y0, w',h'):nextRooms)

makeMap :: [Rect] -> [MapCoord]
makeMap [] = []
makeMap ((x,y,w,h):xs) = ([(x', y', '.') | x' <- [x..(x+w-1)], y' <- [y..(y+h-1)]]) ++ next
  where next = makeMap xs

isInRect :: Int -> Int -> Int -> Int -> [Rect] -> Bool
isInRect _ _ _ _ [] = False
isInRect x y w h (z:zs) = pointInRect x y z w h || isInRect x y w h zs

-- check if a certain point is within the rect and also make sure that no edge is wall-less
pointInRect :: Int -> Int -> Rect -> Int -> Int -> Bool
pointInRect x y (a,b,x0,y0) w h = x >= x0 && y >= y0 && x < x0+a && y < y0 + b && x > 0 && y > 0 && x < w-1 && y < h-1

