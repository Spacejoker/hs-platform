module Model where

import Data.Array

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
  player :: Player,
  physTiles :: Data.Array
}

data Animation = Animation {
  animImageResId :: Int,
  height :: Int,
  width :: Int,
  frameOrder :: [Int],
  frameTime :: Int
}
