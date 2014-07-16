import Test.HUnit

import Model
import Item
import LootGenerator
import MapGenerator

miniLevel = Level [".#","#."] 2 2
freeCellsTest = TestList
  [ "simple getFreeCoords test" ~: [(0,0), (1,1)] ~=? getFreeCoords miniLevel
  ]

levelLootGeneratorTest = TestList
  [ "generate some loot" ~: do ret <- genLevelItems 1 [(x, y) | x <- [0..10], y <- [0..10]] 10 []
                               assert (length ret > 0)
  , "dummy test" ~: True ~=? True
  ]

t = TestList [freeCellsTest, levelLootGeneratorTest]

main = do
  runTestTT t
  -- x <- genMap 50 50 50
  -- mapM_ (putStrLn . show) (lLayout x)

