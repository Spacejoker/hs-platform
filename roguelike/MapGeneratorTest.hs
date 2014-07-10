import Test.HUnit

import Model
import Item
import Monster
import LootGenerator
import MapGenerator (genMap)


main = do
  m <- genMap
  mapM_ putStrLn m
  return ()

