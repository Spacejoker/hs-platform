module Render where

import System.Console.ANSI
import System.IO

import Model

drawFullMap :: [[Char]] -> IO()
drawFullMap level = do
  setSGR [ SetConsoleIntensity BoldIntensity
         , SetColor Foreground Vivid White ]
  let zList = zip [0,1..] level
  mapM_ (\(y, str) -> do setCursorPosition y 0
                         putStrLn str
        ) zList

drawRedraws :: [(Int, Int)] -> [String] -> IO()
drawRedraws _ _ = return()
-- drawRedraws ((x, y):xs) level = do
  -- setCursorPosition y x
  -- let nextChar = (myGetChar (x, y) level)
  -- putStrLn $ [nextChar] 
  -- drawRedraws xs level

drawItems :: [Item] -> IO()
drawItems items = do
  setSGR [ SetConsoleIntensity BoldIntensity
         , SetColor Foreground Vivid Green ]
  mapM_ (\x -> case (iPos x) of
                 Nothing -> return ()
                 Just (x, y) ->  do setCursorPosition y x
                                    putStrLn ")"
        ) items

drawCharacter (heroX, heroY) = do
  setCursorPosition heroY heroX
  setSGR [ SetConsoleIntensity BoldIntensity
         , SetColor Foreground Vivid Red ]
  putStrLn "@"
  setSGR [ SetConsoleIntensity BoldIntensity
         , SetColor Foreground Vivid White ]

drawMobs :: [Mob] -> IO()
drawMobs mobs = do
  setSGR [ SetConsoleIntensity BoldIntensity
         , SetColor Foreground Vivid Yellow ]
  mapM_ (\x -> case (mPos x) of
                 Nothing -> return ()
                 Just (x, y) -> do setCursorPosition y x
                                   putStrLn "G"
        ) mobs
  setSGR [ SetConsoleIntensity BoldIntensity
         , SetColor Foreground Vivid White ]

myGetChar :: Coord -> [MapCoord] -> Char
myGetChar _ [] = '#'
myGetChar (x, y) ((x', y', val):xs)
  | x == x' && y == y' = val
  | otherwise = myGetChar (x, y) xs

