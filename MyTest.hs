import Test.HUnit
import Physics
import Model

testPlayer = Player (Point 100 100)
testTiles = ["0000000000","0000000000","0000000000","0000000000","1111111111"]

nextPosBasic = TestCase (assertEqual "No collision" res (Point 1 0)) 
  where res = nextPos testPlayer testTiles

nextPosTests = TestList [nextPosBasic]

main = runTestTT nextPosTests
