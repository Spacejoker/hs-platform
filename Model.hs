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
  xpos :: Float,
  ypos :: Float
} deriving (Show, Eq)

data Player = Player {
  pos :: Point,
  xspeed :: Float,
  yspeed :: Float,
  gravity :: Float
}

data Gs = Gs {
  running :: Bool,
  res :: Resource,
  player :: Player,
  physTiles :: [[Char]]
}
