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
  gravity :: Float,
  playerAnimations :: [Animation]
}

data Gs = Gs {
  running :: Bool,
  res :: Resource,
  player :: Player,
  physTiles :: [[Char]]
}

data Animation = Animation {
  animImage :: Surface,
  height :: Int,
  width :: Int,
  frameOrder :: [Int],
  frameTime :: Int
}
