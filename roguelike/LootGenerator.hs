module LootGenerator where

import Model
import Item

import System.Random
import Data.Maybe

rollLoot :: Mob -> Float -> Coord -> Item
rollLoot m roll c = axe (Just c)

genLevelItems :: Int -> [Coord] -> Int -> [Item] -> IO[Item]
genLevelItems _ _ 0 cur = return cur
genLevelItems a cand cnt cur = do
  let make = (\x -> x)
  posIdx <- getStdRandom (randomR (2, 6))
  let pos = cand !! posIdx
  let item = axe (Just pos)
  genLevelItems a cand (cnt-1) (item:cur)

