import Test.HUnit

import Model
import Item
import Monster
import LootGenerator
import MapGenerator


main = do
  m <- genMap 50 50 20
  mapM_ putStrLn (lLayout m)
  return ()

