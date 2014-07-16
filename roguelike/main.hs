-- {# LANGUAGE ForeignFunctionInterface #-}
import Data.Char
import Foreign.C.Types

import System.Console.Haskeline
import System.Random

import Model
import DropTree
import Item
import MapGenerator
import LootGenerator
    
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
  startPos <- getRandomFreeCoord level
  let redrawInit = [(x, y) | x <- [0..59], y <- [0..59]]
  genItems <- genLevelItems 0 (getFreeCoords level) 5 []
  gameLoop $ World startPos redrawInit level genItems []

getRandomFreeCoord :: Level -> IO(Coord)
getRandomFreeCoord level@(Level layout mapWidth mapHeight) = do
  let freeCoords = getFreeCoords level
  x <- getStdRandom(randomR(0, (length freeCoords)-1))
  return (freeCoords !! x)

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

gameLoop world@(World hero redraws level items mobs) = do
  drawRedraws redraws (lLayout level)
  drawItems items
  drawCharacter hero
  let world' = world { wRedraw = [] }
  input <- getInput
  let world'' = updateState world' input
  case input of
    Exit -> return ()
    _ -> gameLoop world''

drawRedraws :: [(Int, Int)] -> [[Char]] -> IO()
drawRedraws list level = do
  setSGR [ SetConsoleIntensity BoldIntensity
         , SetColor Foreground Vivid White ]
  mapM_ (\(x, y) -> do setCursorPosition y x
                       putStrLn [((level!! y) !! x)]) list

drawItems :: [Item] -> IO()
drawItems items = do
  setSGR [ SetConsoleIntensity BoldIntensity
         , SetColor Foreground Vivid Green ]
  mapM_ (\x -> case (iPos x) of
                 Nothing -> return ()
                 Just (x, y) ->  do setCursorPosition y x
                                    putStrLn ")"
        ) items

myGetChar :: Coord -> [MapCoord] -> Char
myGetChar _ [] = '#'
myGetChar (x, y) ((x', y', val):xs)
  | x == x' && y == y' = val
  | otherwise = myGetChar (x, y) xs

updateState world@(World hero redraws level items mobs) input = do
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
