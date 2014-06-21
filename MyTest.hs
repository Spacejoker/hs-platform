import Test.HUnit
import Physics
import Model

testPlayer = Player (Point 100 100) 1 0
testTiles = ["0000000000","0000000000","0000000000","0000000000","1111111111"]

nextPosBasic = TestCase (assertEqual "No collision" (Point 101 100) res) 
  where res = nextPos testPlayer testTiles

nextPosTests = TestList [nextPosBasic]

main = runTestTT nextPosTests
