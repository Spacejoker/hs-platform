import Test.HUnit
import Physics
import Model

testPlayer = Player (Point 100 100) 1 0 0.5
testPlayerLeft = Player (Point 100 100) (-1) 0 0.5
testTiles = ["0000000000","0000000000","0000000000","0000000000","1111111111"]
testTilesWithWall = ["0000000000","0000000000","0000000000","0101000000","1111111111"]

nextPosBasic = TestCase (assertEqual "No collision test" (Point 101 100) res) 
  where res = nextPos testPlayer testTiles

nextPosCollission = TestCase (assertEqual "Simple collision test" (Point 100 100) res) 
  where res = nextPos testPlayer testTilesWithWall

nextPosCollissionLeft = TestCase (assertEqual "Simple collision test" (Point 100 100) res) 
  where res = nextPos testPlayerLeft testTilesWithWall

yRangeEdge = TestCase (assertEqual "" [2,3] res)
  where res = affectYRange 100.0

yRangeMid = TestCase (assertEqual "" [1,2,3] res)
  where res = affectYRange 80.0

xRangeMid = TestCase (assertEqual "" [1,2] res)
  where res = affectXRange 80

xRangeEdge = TestCase (assertEqual "" [2] res)
  where res = affectXRange 100

playerHaveYAcc = TestCase (assertBool "" ((gravity testPlayer) /= 0))

nextPosTests = TestList [nextPosBasic, nextPosCollission, nextPosCollissionLeft]
rangeTests = TestList [yRangeMid, yRangeEdge, xRangeMid, xRangeEdge] --, yRangeTestOnEdge]
playerTests = TestList [playerHaveYAcc]

allTests =TestList [nextPosTests, rangeTests, playerTests]

main = runTestTT allTests
