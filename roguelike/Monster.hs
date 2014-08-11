module Monster where

import Model
import MapGenerator
import Data.List (minimumBy)
import Data.Ord (compare)

goblin = Mob "goblin" 1

mobAction :: Mob -> World -> IO(World)
mobAction mob world = do
  let (Just (mx, my)) = mPos mob
      freePos = getFreeCoords (wLevel world)
      cands = [(mx + x, my + y) | x <- [-1,0,1], y <- [-1, 0, 1], x /= 0 || y /= 0, (mx + x, my + y) `elem` freePos]
      (px, py) = wHero world
      hDistList = map (\x -> (dist x (px, py), x)) cands
      (_, ret) = minimumBy (\(a', _) (b', _) -> compare a' b') hDistList
      mob' = mob { mPos = Just ret}
  return world { wMobs = [mob'] }

dist :: Coord -> Coord -> Int
dist (x0, y0) (x1, y1) = min (abs (x1-x0)) (abs (y1-y0))


