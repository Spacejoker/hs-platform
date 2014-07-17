module Monster where

import Model
import MapGenerator

goblin = Mob "goblin" 1

mobAction :: Mob -> World -> IO(World)
mobAction mob world = do
  let (Just (mx, my)) = mPos mob
  let freePos = getFreeCoords (wLevel world)
  let cands = [(mx + x, my + y) | x <- [-1,0,1], y <- [-1, 0, 1], x /= 0 || y /= 0, (mx + x, my + y) `elem` freePos]
  let hDistList = map (\x -> (dist x heroPos, x)) cands
  let (px, py) = wHero world
  let toUnit x = if abs x > 0 then quot x (abs x) else 0
  let newPos = (mx + (toUnit (px - mx)), my + (toUnit (py - my)))
  let mob' = mob { mPos = Just newPos}
  return world { wMobs = [mob'] }

dist :: Coord -> Coord -> Int
dist (x0, y0) (x1, y1) = max (abs (x1-x0)) (abs (y1-y0))


