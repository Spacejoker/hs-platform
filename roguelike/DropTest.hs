import Test.HUnit

import Model
import Item
import Monster
import LootGenerator

dropTests = TestList
  [ "maxRollGoblin" ~: axe ~=? rollLoot (goblin 1) 1.0 ]

main = runTestTT dropTests
