module Model where

import Graphics.UI.SDL as SDL
import Graphics.UI.SDL.TTF as TTF
import Graphics.UI.SDL.Image as SDLi

data Resource = Resource {
  font :: Font,
  test1 :: Surface,
  test2 :: Surface
}

data Point = Point {
  x :: Int,
  y :: Int
}

data Player = Player {
  pos :: Point 
}

data Gs = Gs {
  running :: Bool,
  res :: Resource,
  player :: Player,
  phys_tiles :: [[Char]]
}
