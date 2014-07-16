-- {# LANGUAGE ForeignFunctionInterface #-}
import Data.Char
import Foreign.C.Types

import System.Console.Haskeline
import System.Random

import Model
import DropTree
import Item
import MapGenerator
    
import Prelude hiding (Either(..))

import System.Console.ANSI
import System.IO

getHiddenChar = fmap (chr.fromEnum) c_getch
foreign import ccall unsafe "conio.h getch"
  c_getch :: IO CInt

main = do
  hSetEcho stdin False
  hSetBuffering stdout NoBuffering
  hideCursor
  setTitle "Officelike"
  setSGR [ SetConsoleIntensity BoldIntensity
         , SetColor Foreground Vivid White ]
  clearScreen
  level <- genMap 60 60 50
  startPos <- getLevelFreeSpot level
  let redrawInit = [(x, y) | x <- [0..59], y <- [0..59]]
  gameLoop $ World startPos redrawInit level

getLevelFreeSpot :: Level -> IO(Coord)
getLevelFreeSpot level@(Level layout mapWidth mapHeight) = do
  x <- getStdRandom(randomR(0, mapWidth-1))
  y <- getStdRandom(randomR(0, mapHeight-1))
  if freeTile (x, y) layout
    then return (x, y)
    else getLevelFreeSpot level

-- getLevelFreeSpot :: [[Char]] -> IO(Coord)
-- getLevelFreeSpot c = do
  -- p <- getStdRandom(randomR(0, (length c)-1))
  -- let (x, y, _) = c !! p
  -- return ((x, y))

getInput :: IO(Input)
getInput = do
  char <- getHiddenChar
  case char of
    'q' -> return Exit
    'h' -> return Left
    '4' -> return Left
    't' -> return Down
    '2' -> return Down
    'n' -> return Up
    '8' -> return Up
    's' -> return Right
    '6' -> return Right
    'c' -> return UpRight
    '9' -> return UpRight
    'g' -> return UpLeft
    '7' -> return UpLeft
    'm' -> return DownRight
    '3' -> return DownRight
    'b' -> return DownLeft
    '1' -> return DownLeft
    _ -> getInput

gameLoop world@(World hero redraws level) = do
  drawRedraws redraws (lLayout level)
  let world' = world { wRedraw = [] }
  drawCharacter hero
  input <- getInput
  let world'' = updateState world' input
  case input of
    Exit -> return ()
    _ -> gameLoop world''

drawRedraws :: [(Int, Int)] -> [[Char]] -> IO()
drawRedraws [] _ = return()
drawRedraws ((x, y):tail) level = do
  setCursorPosition y x
  putStrLn [((level !! y) !! x)]
  drawRedraws tail level

--drawRedraws :: [(Int, Int)] -> [MapCoord] -> IO()
--drawRedraws [] _ = return()
--drawRedraws ((x, y):xs) level = do
  --setCursorPosition y x
  --let nextChar = (myGetChar (x, y) level)
  --putStrLn $ [nextChar] 
  --drawRedraws xs level

myGetChar :: Coord -> [MapCoord] -> Char
myGetChar _ [] = '#'
myGetChar (x, y) ((x', y', val):xs)
  | x == x' && y == y' = val
  | otherwise = myGetChar (x, y) xs

updateState world@(World hero redraws level) input = do
  let hero' = handleAction hero input level
  let redraw' = (wRedraw world) ++ [hero]
  world { wHero = hero', wRedraw = redraw' }

drawCharacter (heroX, heroY) = do
  setCursorPosition heroY heroX
  setSGR [ SetConsoleIntensity BoldIntensity
         , SetColor Foreground Vivid Red ]
  putStrLn "@"
  setSGR [ SetConsoleIntensity BoldIntensity
         , SetColor Foreground Vivid White ]
freeTile (x, y) level = levelValue (x, y) level == '.'

levelValue (x, y) level = (level !! y) !! x

-- freeTile :: Coord -> [MapCoord] -> Bool
-- freeTile (x, y) level = (length $ filter (\(x', y', _) -> x' == x && y' == y) level) > 0

handleAction :: Coord -> Input -> Level -> (Int, Int)
handleAction hero@(heroX, heroY) input level = newPos
  where newCoord = case input of
                     Up -> ( heroX, heroY - 1 )
                     Down -> ( heroX, heroY + 1)
                     Left -> ( heroX - 1, heroY ) 
                     Right -> ( heroX + 1, heroY ) 
                     UpRight -> ( heroX + 1, heroY -1 ) 
                     UpLeft -> ( heroX - 1, heroY -1) 
                     DownRight -> ( heroX + 1, heroY +1) 
                     DownLeft -> ( heroX - 1, heroY +1) 
        newPos
          | freeTile newCoord (lLayout level) = newCoord
          | otherwise = (heroX, heroY)
