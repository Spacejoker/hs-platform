import System.Console.Haskeline
import System.Random

import Model
import DropTree
import Item
import MapGenerator
import LootGenerator
import WorldGenerator
import UserInteraction
import Render
    
import Prelude hiding (Either(..))

import System.Console.ANSI
import System.IO

main = do
  hSetEcho stdin False
  hSetBuffering stdout NoBuffering
  hideCursor
  setTitle "Officelike"
  world <- generateWorld 1
  gameLoop world

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


myGetChar :: Coord -> [MapCoord] -> Char
myGetChar _ [] = '#'
myGetChar (x, y) ((x', y', val):xs)
  | x == x' && y == y' = val
  | otherwise = myGetChar (x, y) xs

updateState world@(World hero redraws level items mobs) input = do
  let hero' = handleAction hero input level
  let redraw' = (wRedraw world) ++ [hero]
  world { wHero = hero', wRedraw = redraw' }


