{-# LANGUAGE Arrows #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Prelude hiding ((.), id, flip)

import Control.Wire
import Control.Applicative
import Control.Arrow
import Control.Category ((.), id)
import Control.Lens
import Data.Monoid (mconcat)

import Game
import Graphics
import SDLSession

import qualified Graphics.UI.SDL as SDL
import qualified Graphics.UI.SDL.Keysym as Scancode
import qualified Control.Wire.Unsafe.Event as Unsafe
import qualified FRP.Netwire as FRP

import Linear.Affine
import Linear

--------------------------------------------------------------------------------
main :: IO ()
main =
  play Game { gamePhysicsFramerate = 1 / 200
            , gameWire = marioClone
            , gameQuit = return 
            }


--------------------------------------------------------------------------------
marioClone = pure background <> mario
 where background = clear 255 128 0 255


--------------------------------------------------------------------------------
mario = proc _ -> do
  xVelocity <- keyMap [ (Scancode.Right, 100)
                      , (Scancode.Left, -100)
                      ] <|> 0 -< ()

  position <- (\x -> P (V2 (round (x :: Double)) 300)) <$> FRP.integral 0 -< xVelocity

  idleStateChange <- edge (/= 0) -< xVelocity
  frame <- alternate (pure idleFrame) (cycleFrames running) -< ((), idleStateChange)

  flips <- determineFacing -< xVelocity
  sprite <- marioSprite -< ()
  returnA -< imageRegion sprite frame position flips 

  where
    marioSprite = loadImage "../netwire-classics/platformer/mario.bmp" 255 128 64

    idleFrame = SDL.Rect 4 60 16 28
    
    running = cycle [ SDL.Rect (21 + 16) 60 16 28, SDL.Rect 21 60 16 28, idleFrame, SDL.Rect 21 60 16 28 ]

    determineFacing = proc xVelocity -> do
      rec
        lastFlip <- FRP.delay [] -< flip
        let flip = case signum xVelocity of
                     0 -> lastFlip
                     1 -> []
                     -1 -> [ SDL.Horizontal ]
                     _ -> undefined
      returnA -< flip

    cycleFrames [] = empty
    cycleFrames (a:as) = pure a . for (1 / 8) --> cycleFrames as

