module Main where

import Graphics.UI.SDL as SDL
import Graphics.UI.SDL.TTF as TTF
import Graphics.UI.SDL.Image as SDLi
import Graphics.UI.SDL.Video as SDLv

import System.Random

import Model
import Physics
import Render

get :: Int -> Int -> Int
get x y 
  | y > 5 = 0
  | otherwise = 1

main = do
  SDL.init [InitEverything]

  setVideoMode 800 600 32 []
  TTF.init
  font <- openFont "font.ttf" 30

  setCaption "Platformer" "Platformer" 
  
  let tiles = ["0000000000","0000000000","1001000000","0001000000","1111111111"]
  test1 <- SDLi.load "image/test1.png"
  test2 <- SDLi.load "image/test2.png"
  playerImg <- SDLi.load "image/player.png"
  let p = Player (Point 50 50) 0 0
  let tileset = [test1, test2]
  let r = Resource font tileset playerImg
  let gs = Gs True r p tiles
  s <- getVideoSurface

  title <- renderTextSolid font "Score" (Color 255 0 0)
  blitSurface title Nothing s (Just (Rect 480 60 200 40))
  
  SDL.flip s
  loop gs

processList :: (a -> b -> a) -> a -> [b] -> a
processList _ v [] = v
processList f v (x:xs) = processList f (f v x) xs

loop :: Gs -> IO()
loop gs = do
  
  s <- getVideoSurface

  title <- renderTextSolid (font $ res gs) "Score" (Color 255 0 0)
  blitSurface title Nothing s (Just (Rect 480 60 200 40))
  
  SDL.flip s
  -- SDLv.updateRect s  (Rect 0 0 200 200)
 
  events <- getEvents pollEvent []
  let gs' = tickLogic (processList handleEvent gs events)
  render gs'

  if (running gs')
    then loop gs'
    else return ()

tickLogic :: Gs -> Gs
tickLogic gs = do
  let p = player gs
  let oldx = xpos (pos p)
  let oldy = ypos (pos p)
  let newx = oldx + (xspeed p)
  let newy = oldy
  let pos' = nextPos p (physTiles gs)
  gs { player = (player gs){ pos = pos'} }

getEvents :: IO Event -> [Event] -> IO [Event]
getEvents pEvent es = do
  e <- pEvent
  let hasEvent = e /= NoEvent
  if hasEvent
    then getEvents pEvent (e:es)
    else return (reverse es)

handleEvent :: Gs -> Event -> Gs
handleEvent gs x =
  case x of 
    KeyDown (Keysym SDLK_ESCAPE _ _) -> gs { running = False }
    KeyDown (Keysym SDLK_a _ _) -> gs { player = (player gs) {xspeed = -1} }
    KeyUp (Keysym SDLK_a _ _) -> gs { player = (player gs) {xspeed = 0} }
    KeyDown (Keysym SDLK_e _ _) -> gs { player = (player gs) {xspeed = 1} }
    KeyUp (Keysym SDLK_e _ _) -> gs { player = (player gs) {xspeed = 0} }
    _ -> gs

