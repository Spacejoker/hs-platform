module MapGenerator where

import System.Random
import Model
import Data.List


genMap :: Int -> Int -> Int -> IO(Level)
genMap w h numRooms = do
  rooms <- genRooms numRooms w h []
  let nonOverlap = removeOverlapping rooms []
  let layout = map (makeRow nonOverlap w) [0..(h-1)]
  return (Level layout w h)

makeRow :: [Rect] -> Int -> Int -> String
makeRow rooms w y = [ (\x -> if x == True then '.' else '#') $ roomAt x y rooms | x <- [0..(w-1)] ] 

roomAt :: Int -> Int -> [Rect] -> Bool
roomAt x y rects = any inRect rects
  where inRect = (\(x0, x1, y0, y1) -> x >= x0 && x <= x1 && y >= y0 && y <= y1)

removeOverlapping :: [Rect] -> [Rect] -> [Rect]
removeOverlapping [] ret = ret
removeOverlapping (x : xs) used = removeOverlapping xs inUse
  where inUse
          | 1 == 1 = (x:used) ++ (generateCorridors x used)
          -- | freeRoom x used = (x:used) ++ (generateCorridors x used)
          -- | otherwise = used

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
          
--redo this shit
alterPositions :: Level -> [(Int, Int, Char)] -> Level
alterPositions level@(Level lLayout w h ) changes = level {lLayout = newLayout}
  where newLayout = alterRows lLayout w h changes

alterRows :: [String] -> Int -> Int -> [(Int, Int, Char)] -> [String]
alterRows _ _ 0 _ = []
alterRows (x:xs) w y changes= ((alterRow x w (filter (\(_, y', _) -> y' == y) changes) 0):(alterRows xs w y changes))

alterRow :: String -> Int -> [(Int, Int, Char)] -> Int -> String
alterRow (s:ss) w changes x  
  | x == w = []
  | length thisChange > 0 = (val:rest)
  | otherwise = (s:rest)
  where thisChange = filter (\(x', _, _) -> x' == x) changes
        rest = alterRow ss w changes (x+1)
        (_, _, val) = head thisChange
-- end redo shit

getFreeCoords :: Level -> [(Coord)]
getFreeCoords level = concat ret
  where z = zip [0,1..] (lLayout level)
        ret = map make z

make :: (Int, String) -> [(Coord)]
make (y, s) = ret 
  where z = zip [(0::Int),(1::Int)..] s
        f = filter (\(_, v) -> v /= '#') z
        ret = map (\(x, c) -> (x, y)) f

getRandomFreeCoord :: Level -> IO(Coord)
getRandomFreeCoord level@(Level layout mapWidth mapHeight) = do
  let freeCoords = getFreeCoords level
  putStrLn $ show $ length freeCoords
  x <- getStdRandom(randomR(0, (length freeCoords)-1))
  return (freeCoords !! x)
