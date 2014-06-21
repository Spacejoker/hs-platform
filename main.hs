import Graphics.UI.SDL as SDL
import Graphics.UI.SDL.TTF as TTF
import Graphics.UI.SDL.Image as SDLi

import System.Random

data Resource = Resource {
  font :: Font
}

data Gs = Gs {
  running :: Bool,
  res :: Resource
}

main = do
  SDL.init [InitEverything]

  setVideoMode 800 600 32 []
  TTF.init
  font <- openFont "font.ttf" 30

  setCaption "Platformer" "Platformer" 

  let gs = Gs True (Resource font)
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
 
  events <- getEvents pollEvent []
  let gs' = processList handleEvent gs events
  putStrLn $ show $ length events

  if (running gs')
    then loop gs'
    else return ()

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
    _ -> gs

handle :: Event -> Bool
handle x =
  case x of
    -- KeyDown (Keysym SDLK_SPACE _ _) -> newGameState gs
    -- KeyDown (Keysym SDLK_RETURN _ _) -> case menuchoice gs of
                                          -- 0 -> newGameState gs
                                          -- _ -> gs {gameActive = False}
    KeyDown (Keysym SDLK_ESCAPE _ _) -> False
    _ -> True


