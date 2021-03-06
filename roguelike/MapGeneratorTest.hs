import Test.HUnit

import Model
import Item
import LootGenerator
import MapGenerator
import WorldGenerator
import Monster

import qualified Data.Sequence as Seq

miniLevel = Level [".#","#."] 2 2
freeCellsTest = TestList
  [ "simple getFreeCoords test" ~: [(0,0), (1,1)] ~=? getFreeCoords miniLevel
  ]

levelLootGeneratorTest = TestList
  [ "generate some loot" ~: do ret <- genLevelItems 1 [(x, y) | x <- [0..10], y <- [0..10]] 10 []
                               assert (length ret > 0)
  ]

genMobTest = TestList
  [ "genMobTest" ~: do ret <- genLevelMobs 10 10 [(x, 0) | x <- [0..4]]
                       assert ((length ret) == 5)
  ]

testGoblin = goblin (Just (2, 2))
simpleWorld = World (0,0) [] (Level ["....","....","....","...."] 2 2) [] [testGoblin] Seq.empty
mobActionTest = TestList
  [ "attack player" ~: do w' <- mobAction testGoblin simpleWorld
                          let g' = head $ wMobs w'
                          let (Just x) = mPos g'
                          assertEqual "Moving in open space" (1,1) x
  ]

t = TestList [freeCellsTest, levelLootGeneratorTest, genMobTest, mobActionTest]

main = do
  runTestTT t
  -- x <- genMap 50 50 50
  -- mapM_ (putStrLn . show) (lLayout x)

