module WorldGenerator where

import Model
import Item
import MapGenerator
import LootGenerator

generateWorld :: Int -> IO(World)
generateWorld xl = do
  level <- genMap 60 60 50
  startPos <- getRandomFreeCoord level
  genItems <- genLevelItems 0 (getFreeCoords level) 5 []
  genMobs <- return [] --genLevelMobs 0 5
  let redrawInit = [(x, y) | x <- [0..59], y <- [0..59]]
  return (World startPos redrawInit level genItems [])
