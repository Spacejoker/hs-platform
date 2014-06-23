import Test.HUnit
import Physics
import Model
import Graphics.UI.SDL.Image as SDLi
import Graphics.UI.SDL.Video as SDLv
import Data.Word
import TestRender (renderList)


testPlayer = Player (Point 100 100) 100 0 0.5 []
testPlayerLeft = Player (Point 100 100) (-100) 0 0.5 []
airPlayer = Player (Point 100 50) 0 0 0.5 []

testTiles = ["0000000000","0000000000","0000000000","0000000000","1111111111"]
testTilesWithWall = ["0000000000","0000000000","0000000000","0101000000","1111111111"]

nextPosBasic = TestCase (assertEqual "No collision test" (Point 101 100) res) 
  where res = nextPos testPlayer testTiles 10

nextPosCollission = TestCase (assertEqual "Simple collision test" (Point 100 100) res) 
  where res = nextPos testPlayer testTilesWithWall 10

-- ignore for now
nextPosCollissionHalfWay = TestCase (assertEqual "Improve collission!" (Point 100 100) res) 
  where res = nextPos (Player (Point 99 100) 200 0 0.5 []) testTilesWithWall 10

nextPosCollissionLeft = TestCase (assertEqual "Simple collision test left" (Point 100 100) res) 
  where res = nextPos testPlayerLeft testTilesWithWall 10

--added
pFalling = TestCase (assertEqual "Add falling-collission" (Point 100 100) res)
  where res = nextPos (Player (Point 100 99) 0 100 0.5 []) testTilesWithWall 20

--added
pApplyGravity = TestCase (assertEqual "Create gravity f" 1.0 res)
  where res = yspeed (applyGravity airPlayer 20)

--added
pJumpInAir = TestCase ( assertBool "Jump player in air" (res >= 0))
  where res = yspeed $ jump airPlayer False

--added
pJumpOnGround = TestCase ( assertBool "Jump player on ground" (res < 0))
  where res = yspeed $ jump testPlayer True

--added
pAccellerateDown = TestCase ( assertBool "Longer dt -> higher acceleration" (a > b) )
  where a = yspeed ( applyGravity airPlayer 100)
        b = yspeed ( applyGravity airPlayer 10)

yRangeEdge = TestCase (assertEqual "yRangeEdge" [2,3] res)
  where res = affectYRange 100.0

yRangeMid = TestCase (assertEqual "yRangeMid" [1,2,3] res)
  where res = affectYRange 80.0

xRangeEdge = TestCase (assertEqual "xRangeEdge" [2] res)
  where res = affectXRange 100

xRangeMid = TestCase (assertEqual "xRangeMid" [1,2] res)
  where res = affectXRange 80

playerHaveYAcc = TestCase (assertBool "" ((gravity testPlayer) /= 0))

ignoredTests = TestList []
nextPosTests = TestList [nextPosCollissionHalfWay, nextPosBasic, nextPosCollission, nextPosCollissionLeft]
rangeTests = TestList [yRangeMid, yRangeEdge, xRangeMid, xRangeEdge] 
gravityTests = TestList [pApplyGravity, pFalling]
jumpTests = TestList [pJumpInAir, pJumpOnGround]
playerTests = TestList [playerHaveYAcc]

physTests = TestList [nextPosTests, rangeTests, playerTests, gravityTests, jumpTests]

allTests =TestList [physTests, renderList]
main = runTestTT allTests

