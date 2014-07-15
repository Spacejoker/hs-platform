import Test.HUnit

import Model
import Item
import Monster
import LootGenerator
import MapGenerator

f x
  | x == 2 = '#'
  | otherwise = '.'
smallSquare = [(x, y, (\v -> if v == 2 then '#' else '.') x) | x <- [0..4], y <- [0..4]]

twoSmallRooms = [(0,0,'.'), (2,2,'.')]

largeSize = 100
largeConnected = [(x, y, '.') | x <- [0..(largeSize-1)], y <- [0..(largeSize -1)]]

dfsTest = TestList
  [ 
    "DFS basic" ~: 10 ~=? length (dfs smallSquare [(0,0,'.')] []) 
  , "DFS large" ~: largeSize ^ 2 ~=? length (dfs largeConnected [(0,0,'.')] []) 
  ]

dim = 40
roomlen = 15

largeMap = [(x, y, '.') | x <- [0..dim], y <- [0..dim], (x < roomlen && y < roomlen) || (x>(dim - roomlen) && y > (dim - roomlen))]

connectRoomTest = TestList 
  [ "connect two small rooms" ~: 5 ~=? length ( connectRooms twoSmallRooms 1 )
  , "connect large map" ~: True ~=? length (connectRooms largeMap 1) > roomlen*roomlen*2
  ]

t = TestList [dfsTest, connectRoomTest]

main = do
  -- runTestTT t
  --let ans = dfs largeConnected [(0,0,'.')] []
  --putStrLn $ show $ head ans
  --return (ans)
  x <- genMap 50 50 50
  mapM_ (putStrLn . show) x
  -- putStrLn $ show x

