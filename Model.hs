module Model where

import Graphics.UI.SDL as SDL
import Graphics.UI.SDL.TTF as TTF
import Graphics.UI.SDL.Image as SDLi

data Resource = Resource {
  font :: Font,
  tileset :: [Surface],
  playerImg :: Surface
}

data Point = Point {
  xpos :: Int,
  ypos :: Int
}

data Player = Player {
  pos :: Point,
  xspeed :: Int,
  yspeed :: Int
}

data Gs = Gs {
  running :: Bool,
  res :: Resource,
  player :: Player,
  physTiles :: [[Char]]
}
