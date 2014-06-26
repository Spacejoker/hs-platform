module Model where

data Point = Point {
  xpos :: Float,
  ypos :: Float
} deriving (Show, Eq)

data Player = Player {
  pos :: Point,
  xspeed :: Float,
  yspeed :: Float,
  gravity :: Float,
  playerAnimations :: [Animation],
  playerSurfaceId :: SurfaceId
}

data Gs = Gs {
  running :: Bool,
  player :: Player,
  physTiles :: [[Char]],
  graphics :: [SurfaceId]
}

data Animation = Animation {
  animSurfaceId :: SurfaceId,
  height :: Int,
  width :: Int,
  frameOrder :: [Int],
  frameTime :: Int
}

data SurfaceId = Tile1 | Tile2 | PlayerSprite
  deriving (Enum)
