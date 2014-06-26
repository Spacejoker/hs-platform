module Render where

import Graphics.UI.SDL as SDL
import Data.Char (digitToInt)

import Model
import ModelSdl

dim :: Int
dim = 50

getFrame :: Animation -> Int -> Int
getFrame animation t = 0 -- (t `quot` (frameTime animation)) `mod` (length $ frameOrder animation)

--getPlayerAnimImage :: Player -> Int -> (Surface, Rect)
--getPlayerAnimImage p t = (animImage animation, Rect (frame*w) 0 (frame*(w+1)) 100)
  --where animation = head (playerAnimations p)
        --frame = getFrame animation t
        --w = width animation

render :: Gs -> Graphics -> Int -> IO()
render gs g t = do
  renderRows 0 0 g (physTiles gs)
  renderPlayer ((surfaces g) !! 2, Rect 0 0 0 0) (player gs)
  -- renderPlayer (getPlayerAnimImage (player gs) t ) (player gs)

renderPlayer :: (Surface, Rect) -> Player -> IO()
renderPlayer (img, rect) p = do
  s <- getVideoSurface
  let (xp, yp) = (\x -> (floor $ xpos x - 25, floor $ ypos x)) (pos p)
  blitSurface img Nothing s (Just (Rect xp yp (xp + dim) (yp + dim*2)))
  -- blitSurface img (Just rect) s (Just (Rect xp yp (xp + dim) (yp + dim*2)))
  return ()

renderRows :: Int -> Int -> Graphics -> [[Char]] -> IO()
renderRows _ _ _ [] = return ()
renderRows x y res (r:rs) = do
  renderTile x y res r
  renderRows x (y+1) res rs

renderTile :: Int -> Int -> Graphics -> [Char] -> IO()
renderTile _ _ _ [] = return ()
renderTile x y res (c:cs) = do

  s <- getVideoSurface

  let v = digitToInt c
  let xx = x*dim
  let yy = y*dim
  let img = (surfaces res) !! v
  
  blitSurface img Nothing s (Just (Rect xx yy (xx + dim) (yy+dim)))
  renderTile (x+1) y res cs

