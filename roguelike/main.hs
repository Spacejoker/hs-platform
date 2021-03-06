import System.Console.Haskeline
import System.Random
import Data.Sequence as Seq

import Model
import DropTree
import Item
import MapGenerator
import LootGenerator
import WorldGenerator
import UserInteraction
import Render
import Monster
    
import Prelude hiding (Either(..))

import System.Console.ANSI
import System.IO

main = do
  -- putStrLn "test"
  hSetEcho stdin False
  hSetBuffering stdout NoBuffering
  hideCursor
  setTitle "Officelike"
  world <- generateWorld 1
  render world
  gameLoop world

gameLoop world@(World hero redraws level items mobs queue) = do
  let (nextEvent :< restOfQueue) = Seq.viewl queue
  case aType nextEvent of 
    PlayerActionEvent -> do let newQueue = (><) restOfQueue (Seq.singleton nextEvent)
                            w' <- playerAction world { wActQueue = newQueue}
                            gameLoop w'
    MobActionEvent    -> do let newQueue = (><) restOfQueue (Seq.singleton nextEvent)
                            let Just idx = aMobId nextEvent
                            let mob = head mobs 
                            w' <- mobAction mob (world { wActQueue = newQueue} )
                            gameLoop w'
    _                 -> gameLoop (world { wActQueue = newQueue } )
                           where newQueue = (><) restOfQueue (Seq.singleton nextEvent)

render :: World -> IO()
render world = do
  drawFullMap $ lLayout $ wLevel world
  drawItems $ wItems world
  drawCharacter $ wHero world
  drawMobs $ wMobs world
  

playerAction :: World -> IO(World)
playerAction world = do
  input <- getInput
  let world' = handleAction world input

  render world'
  return world'

