module WorldGenerator where

import System.Random
import Data.Array.IO
import Data.Sequence(Seq, (><), singleton, fromList)
import Control.Monad

import Model
import Item
import MapGenerator
import LootGenerator


generateWorld :: Int -> IO(World)
generateWorld xl = do

  level <- genMap 60 60 50

  startPos <- getRandomFreeCoord level

  let freeCoords = (getFreeCoords level)
  shuffledCoords <- shuffle freeCoords

  genItems <- genLevelItems 0 shuffledCoords 5 []
  genMobs <- genLevelMobs 0 5 shuffledCoords
  
  let mobActions = map (\(id, _) -> ActionEvent MobActionEvent (Just id)) (zip [0,1..] genMobs)
  let actionQueue = (><) (singleton (ActionEvent PlayerActionEvent Nothing)) (fromList mobActions)

  let redrawInit = [(x, y) | x <- [0..59], y <- [0..59]]
  return (World startPos redrawInit level genItems genMobs actionQueue)

-- stolen function from hackage
shuffle :: [a] -> IO [a]
shuffle xs = do
        ar <- newArray n xs
        forM [1..n] $ \i -> do
            j <- randomRIO (i,n)
            vi <- readArray ar i
            vj <- readArray ar j
            writeArray ar j vi
            return vj
  where
    n = length xs
    newArray :: Int -> [a] -> IO (IOArray Int a)
    newArray n xs =  newListArray (1,n) xs

genLevelMobs :: Int -> Int -> [Coord] -> IO[Mob]
genLevelMobs xl cnt coords = do
  mobs <- sequence $ take cnt $ repeat (genMob xl)
  let tup = zip coords mobs
  return $ (map (\(pos, mob) -> mob (Just pos)) tup)

genMob :: Int -> IO(Maybe Coord -> Mob)
genMob xl = do
  
  x <- getStdRandom ( randomR( xl - 1, xl + 1))
  return rat
  
