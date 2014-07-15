module MapGenerator where

import System.Random
import Model
import Data.List


genMap :: Int -> Int -> Int -> IO([[Char]])
genMap w h numRooms = do
  rooms <- genRooms numRooms w h []
  let nonOverlap = removeOverlapping rooms []
  return (map (makeRow nonOverlap w) [0..(h-1)])

makeRow :: [Rect] -> Int -> Int -> String
makeRow rooms w y = [ (\x -> if x == True then '.' else '#') $ roomAt x y rooms | x <- [0..(w-1)] ] 

roomAt :: Int -> Int -> [Rect] -> Bool
roomAt x y rects = any inRect rects
  where inRect = (\(x0, x1, y0, y1) -> x >= x0 && x <= x1 && y >= y0 && y <= y1)

removeOverlapping :: [Rect] -> [Rect] -> [Rect]
removeOverlapping [] ret = ret
removeOverlapping (x : xs) used = removeOverlapping xs inUse
  where inUse
          | freeRoom x used = (x:used) ++ (generateCorridors x used)
          | otherwise = used

generateCorridors :: Rect -> [Rect] -> [Rect]
generateCorridors _ [] = []
generateCorridors (x1, _, y1, _) ((x2, _, y2, _):xs) = [(x1, x2, y1 ,y1), (x2, x2, y1, y2)]


freeRoom :: Rect -> [Rect] -> Bool
freeRoom _ [] = True
freeRoom r ( x : xs)
  | rectOverlap r x = False
  | otherwise = freeRoom r xs

rectOverlap :: Rect -> Rect -> Bool
rectOverlap (x1, x2, y1, y2) (otherx1, otherx2, othery1, othery2) = ans
    where ans = x1 < otherx2 && x2 > otherx1 && y1 < othery2 && y2 > othery1

genRooms :: Int -> Int -> Int -> [Rect] -> IO( [Rect] )
genRooms 0 _ _ current = return current
genRooms i w h current = do
  w' <- getStdRandom (randomR (2, 6))
  h' <- getStdRandom (randomR (2, 6))
  x0 <- getStdRandom (randomR (0, w - w'))
  y0 <- getStdRandom (randomR (0, h - h'))
  genRooms (i-1) w h ((x0, x0 + w' - 1, y0, y0 + h' - 1):current)
          
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

