module UserInteraction where 
-- {# LANGUAGE ForeignFunctionInterface #-}
import Data.Char
import Foreign.C.Types
import Model
import Prelude hiding (Either(..))

getHiddenChar = fmap (chr.fromEnum) c_getch
foreign import ccall unsafe "conio.h getch"
  c_getch :: IO CInt

getInput :: IO(Input)
getInput = do
  char <- getHiddenChar
  case char of
    'q' -> return Exit
    'h' -> return Left
    '4' -> return Left
    't' -> return Down
    '2' -> return Down
    'n' -> return Up
    '8' -> return Up
    's' -> return Right
    '6' -> return Right
    'c' -> return UpRight
    '9' -> return UpRight
    'g' -> return UpLeft
    '7' -> return UpLeft
    'm' -> return DownRight
    '3' -> return DownRight
    'b' -> return DownLeft
    '1' -> return DownLeft
    _ -> getInput

handleAction :: World -> Input -> World
handleAction world input = world { wHero = newPos }
  where newCoord = case input of
                     Up -> ( heroX, heroY - 1 )
                     Down -> ( heroX, heroY + 1)
                     Left -> ( heroX - 1, heroY ) 
                     Right -> ( heroX + 1, heroY ) 
                     UpRight -> ( heroX + 1, heroY -1 ) 
                     UpLeft -> ( heroX - 1, heroY -1) 
                     DownRight -> ( heroX + 1, heroY +1) 
                     DownLeft -> ( heroX - 1, heroY +1) 
        (heroX, heroY) = wHero world
        newPos
          | freeTile newCoord (lLayout $ wLevel world) = newCoord
          | otherwise = (heroX, heroY)

freeTile :: Coord -> [[Char]] -> Bool
freeTile (x, y) level = levelValue (x, y) level == '.'

levelValue :: Coord -> [[Char]] -> Char
levelValue (x, y) level = (level !! y) !! x
