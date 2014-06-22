
module TestRender where

import Graphics.UI.SDL as SDL
import Graphics.UI.SDL.TTF as TTF
import Graphics.UI.SDL.Image as SDLi
import Graphics.UI.SDL.Video as SDLv
import Data.Word(Word32)

import Test.HUnit

import Render
import Model


testSurface = SDLv.createRGBSurface [] 0 0 0 (0::Word32) (0::Word32) (0::Word32) (0::Word32) 

testAnim s = Animation s 1 1 [0,1] 100

correctAnimationFrame = do 
  testSurface <- SDLi.load "image/test1.png"
  assertEqual "" 1 (getFrame (testAnim testSurface) 99)

renderList = TestList["anim test " ~: correctAnimationFrame]
