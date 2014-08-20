import Test.HUnit

import Model
import Item
import LootGenerator
import MapGenerator
import WorldGenerator
import Monster

import qualified Data.Sequence as Seq

miniLevel = Level [".#","#."] 2 2
smallOpenLevel = Level ["....","....","....","...."] 4 4
smallPillarLevel = Level ["....",".#..","....","...."] 4 4

testGoblin = goblin (Just (2, 2))

simpleWorld = World (0,0) [] smallOpenLevel [] [testGoblin] Seq.empty
twoMobsWorld = World (0,0) [] smallOpenLevel [] [goblin (Just (1, 1)), testGoblin] Seq.empty

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


mobActionTest = TestList
  [ "move towards player" ~: do w' <- mobAction testGoblin simpleWorld
                                let g' = head $ wMobs w'
                                let (Just x) = mPos g'
                                assertEqual "" (1,1) x
  , "move aronud obstacle" ~: do w' <- mobAction testGoblin (simpleWorld { wLevel = smallPillarLevel } )
                                 let g' = head $ wMobs w'
                                 let (Just x) = mPos g'
                                 assertEqual "" True (elem x [(2,1), (1,2)])
  , "One mob per square" ~: do w' <- mobAction testGoblin simpleWorld
                               assertEqual "" 2 ((length . wMobs) w') -- make sure no mobs removed
  ]

t = TestList [freeCellsTest, levelLootGeneratorTest, genMobTest, mobActionTest]

main = do
  runTestTT t
  -- x <- genMap 50 50 50
  -- mapM_ (putStrLn . show) (lLayout x)

