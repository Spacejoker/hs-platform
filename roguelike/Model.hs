module Model where

data Tree a = Leaf a | Node String [Tree a] deriving (Show, Read, Eq)

data Item = Item {
  itemName :: String,
  itemLevel :: Int,
  dropMod :: Float
} deriving (Show, Eq)

data Monster = Monster {
  monsterName :: String,
  monsterLevel :: Int
} deriving (Show, Eq)

type Rect = (Int, Int, Int, Int)

type Coord = (Int, Int)

data World = World {
  wHero :: Coord,
  wRedraw :: [Coord],
  wLevel :: [[Char]]
}

data Input = Up
           | Down
           | Left
           | Right
           | Exit
           deriving (Eq, Show)
