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
  level <- genMap 60 60 30
  startingPosition <- getLevelFreeSpot level
  drawLevelMap 0 (lLayout level)
  gameLoop $ World startingPosition [] (lLayout level)

getLevelFreeSpot :: Level -> IO(Coord)
getLevelFreeSpot level@(Level layout mapWidth mapHeight) = do
  x <- getStdRandom(randomR(0, mapWidth-1))
  y <- getStdRandom(randomR(0, mapHeight-1))
  if freeTile (x, y) layout
    then return (x, y)
    else getLevelFreeSpot level

getInput :: IO(Input)
getInput = do
  char <- getHiddenChar
  case char of
    'q' -> return Exit
    'h' -> return Left
    't' -> return Down
    'n' -> return Up
    's' -> return Right
    _ -> getInput

gameLoop world@(World hero redraws level) = do
  drawRedraws redraws level
  drawCharacter hero
  input <- getInput
  let world' = updateState world input
  case input of
    Exit -> return ()
    _ -> gameLoop world'

drawRedraws :: [(Int, Int)] -> [[Char]] -> IO()
drawRedraws [] _ = return()
drawRedraws ((x, y):tail) level = do
  setCursorPosition y x
  putStrLn [((level !! y) !! x)]
  drawRedraws tail level

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

drawLevelMap _ [] = return ()
drawLevelMap y (z:zs) = do
  setCursorPosition y 0
  putStrLn z
  drawLevelMap (y+1) zs
  

testMap = 
  [ "####################"
  , "#..................#"
  , "#......##..........#"
  , "#.....#............#"
  , "#..................#"
  , "#..................#"
  , "#..................#"
  , "#..................#"
  , "#..................#"
  , "#..................#"
  , "#..................#"
  , "#..................#"
  , "#..................#"
  , "#..................#"
  , "#..................#"
  , "#..................#"
  , "#..................#"
  , "#..................#"
  , "#..................#"
  , "#..................#"
  , "####################"
  ]

freeTile (x, y) level = levelValue (x, y) level == '.'

levelValue (x, y) level = (level !! y) !! x


handleAction hero@(heroX, heroY) input level = newPos
  where newCoord = case input of
                     Up -> ( heroX, heroY - 1 )
                     Down -> ( heroX, heroY + 1)
                     Left -> ( heroX - 1, heroY ) 
                     Right -> ( heroX + 1, heroY ) 
        newPos
          | freeTile newCoord level = newCoord
          | otherwise = (heroX, heroY)
