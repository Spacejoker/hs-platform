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
  hSetEcho stdin False
  hSetBuffering stdout NoBuffering
  hideCursor
  setTitle "Officelike"
  world <- generateWorld 1
  gameLoop world

gameLoop world@(World hero redraws level items mobs queue) = do
  let (nextEvent :< restOfQueue) = Seq.viewl queue
  case aType nextEvent of 
    PlayerActionEvent -> do let newQueue = (><) restOfQueue (Seq.singleton nextEvent)
                            w' <- playerAction world { wActQueue = newQueue}
                            gameLoop w'
    MobActionEvent    -> do let newQueue = (><) restOfQueue (Seq.singleton nextEvent)
                            let Just idx = aMobId nextEvent
                            let mob = mobs !! idx
                            w' <- mobAction mob (world { wActQueue = newQueue} )
                            gameLoop w'
    _                 -> gameLoop (world { wActQueue = newQueue } )
                           where newQueue = (><) restOfQueue (Seq.singleton nextEvent)

playerAction :: World -> IO(World)
playerAction world = do
  -- drawRedraws redraws (lLayout level)
  input <- getInput
  let world' = handleAction world input

  -- render here
  drawRedraws [] $ lLayout $ wLevel world
  drawItems $ wItems world
  drawCharacter $ wHero world
  drawMobs $ wMobs world

  return world'

