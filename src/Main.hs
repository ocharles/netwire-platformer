{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
import Prelude hiding ((.), id)
import Control.Applicative
import Control.Arrow ((&&&))
import Control.Category ((.), id)
import Data.Time (NominalDiffTime, diffUTCTime, getCurrentTime)

import qualified Control.Wire as Netwire

import SDLSession

import qualified Graphics.UI.SDL as SDL
import qualified Graphics.UI.SDL.Keysym as Scancode

--------------------------------------------------------------------------------
type Wire e a b = Netwire.Wire Session e IO a b

type Session = SDLSession (Netwire.Timed Netwire.NominalDiffTime ())

data Game =
  forall frame exitCode.
  Game { gameWire :: forall a. Wire exitCode () frame
       , gamePhysicsFramerate :: NominalDiffTime
       , gameRender :: frame -> IO ()
       , gameQuit :: exitCode -> IO ()
       }

--------------------------------------------------------------------------------
simulate
  :: Netwire.NominalDiffTime -> Wire e () b -> (b -> IO ()) -> IO e
simulate timeStep initialWire render = do
  -- Step once to obtain an initial frame
  (r, w, s) <- do
    (ds, s) <- Netwire.stepSession session
    (r, w) <- Netwire.stepWire initialWire ds (Right ())
    return (r, w, s)

  now <- getCurrentTime
  go w s 0 now r

  where
    session = sdlSession <*> Netwire.countSession_ timeStep

    go wire' session' leftOver' previousTime lastOutput = do
      currentTime <- getCurrentTime
      let delta = leftOver' + currentTime `diffUTCTime` previousTime
          steps = floor (delta / timeStep) :: Int
          leftOver = delta - (fromIntegral steps * timeStep)

      (output, wire, session) <-
        stepNTimes lastOutput wire' session' (min 25 steps)

      either
        return
        (\x -> render x >> go wire session leftOver currentTime output)
        output

    stepNTimes r' w' s' steps =
      if steps <= 0
         then return (r', w', s')
          else do
            (ds, s) <- Netwire.stepSession s'
            (r, w) <- Netwire.stepWire w' ds (Right ())
            stepNTimes r w s (steps - 1)

--------------------------------------------------------------------------------
play :: Game -> IO ()
play Game {..} =
  SDL.withInit [SDL.InitEverything] $
  SDL.withWindow "Mario" (SDL.Position 0 0) (SDL.Size 800 600) [] $ \window ->
  SDL.withRenderer window SDL.FirstSupported [SDL.Accelerated] $ \renderer ->
  simulate gamePhysicsFramerate gameWire gameRender >>= gameQuit

--------------------------------------------------------------------------------
main :: IO ()
main = play Game { gamePhysicsFramerate = 1 / 200
                 , gameWire = Netwire.until . ("OK" &&& id) . keyDown Scancode.Right
                 , gameRender = print
                 , gameQuit = return 
                 }
