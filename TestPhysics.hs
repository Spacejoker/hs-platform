import Test.HUnit
import Physics
import Model
import Data.Word

-- test players
testPlayer = Player (Point 100 100) 100 0 0.5 []
testPlayerLeft = Player (Point 100 100) (-100) 0 0.5 []
airPlayer = Player (Point 100 50) 0 0 0.05 []
downPlayer = Player (Point 100 95) 0 100 0.05 []
edgePlayer = Player (Point 20 0) 0 100 0.05 []

--test maps
testTiles = ["0000000000","0000000000","0000000000","0000000000","1111111111"]
testTilesWithWall = ["0000000000","0000000000","0000000000","0101000000","1111111111"]

nextPosTests = TestList 
    [ "No collision test" ~: (Point 101 100) ~=? nextPos testPlayer testTiles 10
    , "Simple collision test" ~: (Point 100 100) ~=? nextPos testPlayer testTilesWithWall 10
    , "Improve collission!" ~: (Point 100 100) ~=? nextPos (Player (Point 99 100) 200 0 0.5 []) testTilesWithWall 10
    , "Simple collision test left" ~: (Point 100 100) ~=? nextPos testPlayerLeft testTilesWithWall 10
    , "Simple y-collission" ~: (Point 100 100) ~=? nextPos downPlayer testTiles 100
    , "y collission on edge" ~: (Point 20 1) ~=? nextPos downPlayer ["000000","000000","011111","111111"] 100
    ]

findFirstTest = TestList
    [ "basic find first" ~: 49.0 ~=? findFirstFree 55.0 (-1.0) 10.0 ["00","11"]
    ]

gravityTests = TestList 
    [ "Test apply gravity" ~: 1.0 ~=? (yspeed (applyGravity airPlayer 20))
    , "Falling" ~: (Point 100 100) ~=? nextPos (Player (Point 100 99) 0 100 0.5 []) testTilesWithWall 20
    ]

jumpTests = TestList
    [ "Jump player in air" ~: assertBool "" ((yspeed $ jump airPlayer testTiles) >= 0)
    , "Jump player on ground" ~: assertBool "" ((yspeed $ jump testPlayer testTiles) < 0)
    ]

playerTests = TestList
    [  "Longer dt -> higher acceleration" ~: True ~=? (f 100 > f 10)
    ]
  where f = (\x -> yspeed ( applyGravity airPlayer x))

rangeTests = TestList 
    [ "yRangeEdge" ~: [2,3] ~=? affectYRange 100.0
    , "yRangeMid" ~: [1,2,3] ~=? affectYRange 80.0
    , "xRangeEdge" ~: [2] ~=? affectXRange 100
    , "xRangeMid" ~: [1,2] ~=? affectXRange 80
    ]

playerHaveYAcc = TestCase (assertBool "" ((gravity testPlayer) /= 0))

collissionTests = TestList
     [ "collide1" ~: True ~=? isTileColliding [(0, 0), (2, 1)] ["0000","0010","0000"]
     , "collide1" ~: False ~=? isTileColliding [(2, 1)] ["0000","0000","0100"]
     ]

pixelCheck = TestList
    [ "pixelWall1" ~: True ~=? pixelWall (Point 0 0) ["10","00"]
    , "pixelWall2" ~: True ~=? pixelWall (Point 40 40) ["10","00"]
    , "pixelWall3" ~: False ~=? pixelWall (Point 50 50) ["10","00"]
    , "pixelWall4" ~: False ~=? pixelWall (Point 200 199) testTiles
    , "pixelWall5" ~: True ~=? pixelWall (Point 10.0 55.0) ["00","11"]
    ]

physTests = TestList [nextPosTests, rangeTests, playerTests, gravityTests, jumpTests, pixelCheck, collissionTests, findFirstTest]

allTests =TestList [physTests]
main = runTestTT allTests

