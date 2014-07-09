{-# LANGUAGE ForeignFunctionInterface #-}
import Data.Char
import Foreign.C.Types

import System.Console.Haskeline

import Model
import DropTree
import Item
    
import Prelude hiding (Either(..))

import System.Console.ANSI
import System.IO

getHiddenChar = fmap (chr.fromEnum) c_getch
foreign import ccall unsafe "conio.h getch"
  c_getch :: IO CInt

type Coord = (Int, Int)

data World = World {
  wHero :: Coord,
  wRedraw :: [Coord]
}

data Input = Up
           | Down
           | Left
           | Right
           | Exit
           deriving (Eq, Show)

main = do
  hSetEcho stdin False
  hSetBuffering stdout NoBuffering
  hideCursor
  setTitle "Officelike"
  setSGR [ SetConsoleIntensity BoldIntensity
         , SetColor Foreground Vivid White ]
  clearScreen
  drawLevelMap 0 levelMap
  gameLoop $ World (3, 3) []

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

gameLoop world@(World hero redraws) = do
  drawRedraws redraws
  drawCharacter hero
  input <- getInput
  let world' = updateState world input
  case input of
    Exit -> return ()
    _ -> gameLoop world'

drawRedraws :: [(Int, Int)] -> IO()
drawRedraws [] = return()
drawRedraws ((x, y):tail) = do
  setCursorPosition y x
  putStrLn [((levelMap !! y) !! x)]
  drawRedraws tail

updateState world@(World hero redraws) input = do
  let hero' = handleAction hero input
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
  

levelMap = 
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

freeTile (x, y) = x >= 0 && x <= 20 && y >= 0 && y < 20 && levelValue (x, y) == '.'

levelValue (x, y) = (levelMap !! y) !! x


handleAction hero@(heroX, heroY) input = newPos
  where newCoord = case input of
                     Up -> ( heroX, heroY - 1 )
                     Down -> ( heroX, heroY + 1)
                     Left -> ( heroX - 1, heroY ) 
                     Right -> ( heroX + 1, heroY ) 
        newPos
          | freeTile newCoord = newCoord
          | otherwise = (heroX, heroY)
