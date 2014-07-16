module Render where

import System.Console.ANSI
import System.IO

import Model

drawRedraws :: [(Int, Int)] -> [[Char]] -> IO()
drawRedraws list level = do
  setSGR [ SetConsoleIntensity BoldIntensity
         , SetColor Foreground Vivid White ]
  mapM_ (\(x, y) -> do setCursorPosition y x
                       putStrLn [((level!! y) !! x)]) list

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
