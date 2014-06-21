module Render where
import Graphics.UI.SDL as SDL

import Model

dim :: Int
dim = 50

render :: Gs -> IO()
render gs = do
  let t = physTiles gs
  mapM_ (renderTile (res gs)) t
  
renderTile :: Resource -> (Int, Int, Int) -> IO()
renderTile res (x, y, v) = do

  s <- getVideoSurface
  let xx = x*dim
  let yy = y*dim
  let img = (tileset res) !! v
  
  blitSurface img Nothing s (Just (Rect xx yy (xx + dim) (yy+dim)))
  return ()

