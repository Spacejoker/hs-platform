module Render where
import Graphics.UI.SDL as SDL
import Data.Char (digitToInt)

import Model

dim :: Int
dim = 50

render :: Gs -> IO()
render gs = do
  let t = physTiles gs
  renderRows 0 0 (res gs) t
  -- mapM_ (renderTile (res gs)) t
  renderPlayer (playerImg $ res gs) (player gs)
  

renderPlayer :: Surface -> Player -> IO()
renderPlayer img p = do
  s <- getVideoSurface
  let (xp, yp) = (\x -> (xpos x, ypos x)) (pos p)
  blitSurface img Nothing s (Just (Rect xp yp (xp + dim) (yp + dim*2)))
  return ()

renderRows :: Int -> Int -> Resource -> [[Char]] -> IO()
renderRows _ _ _ [] = return ()
renderRows x y res (r:rs) = do
  renderTile x y res r
  renderRows x (y+1) res rs


renderTile :: Int -> Int -> Resource -> [Char] -> IO()
renderTile _ _ _ [] = return ()
renderTile x y res (c:cs) = do

  s <- getVideoSurface

  let v = digitToInt c
  let xx = x*dim
  let yy = y*dim
  let img = (tileset res) !! v
  putStrLn $ show xx
  
  blitSurface img Nothing s (Just (Rect xx yy (xx + dim) (yy+dim)))
  renderTile (x+1) y res cs

-- renderTile :: Resource -> (Int, Int, Int) -> IO()
-- renderTile res (x, y, v) = do
-- 
  -- s <- getVideoSurface
  -- let xx = x*dim
  -- let yy = y*dim
  -- let img = (tileset res) !! v
  -- 
  -- blitSurface img Nothing s (Just (Rect xx yy (xx + dim) (yy+dim)))
  -- return ()
