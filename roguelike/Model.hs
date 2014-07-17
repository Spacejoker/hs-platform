module Model where

import Data.Maybe
import Data.Sequence

type Rect = (Int, Int, Int, Int)
type Coord = (Int, Int)
type MapCoord = (Int, Int, Char)

-- used for drop structure
data Tree a = Leaf a | Node String [Tree a] deriving (Show, Read, Eq)

data Item = Item {
  iName :: String,
  iType :: ItemType,
  iDesc :: String,
  iPos :: Maybe Coord
} deriving (Show, Eq)

data Mob = Mob {
  mName :: String,
  mLevel :: Int,
  mPos :: Maybe Coord
} deriving (Show, Eq)

data Level = Level {
  lLayout :: [[Char]],
  lWidth :: Int,
  lHeight :: Int
}  

data World = World {
  wHero :: Coord,
  wRedraw :: [Coord],
  wLevel :: Level,
  wItems :: [Item],
  wMobs :: [Mob],
  wActQueue :: Seq ActionEvent
}

data ActionEventType = PlayerActionEvent
                     | MobActionEvent
                     deriving (Eq, Show)

data ActionEvent = ActionEvent {
  aType :: ActionEventType,
  aMobId :: Maybe Int
} deriving (Eq, Show)


data Input = Up
           | Down
           | Left
           | Right
           | UpRight
           | UpLeft
           | DownRight
           | DownLeft
           | Exit
           deriving (Eq, Show)

data ItemType = Weapon
              | Potion
              deriving (Eq, Show)
