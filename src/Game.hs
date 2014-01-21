{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
module Game (Wire, Session, GameMonad, Game(..), play) where

import Prelude hiding ((.), id)
import Control.Applicative
import Control.Arrow ((&&&))
import Control.Category ((.), id)
import Control.Lens
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.RWS
import Data.Monoid
import Data.Time (NominalDiffTime, diffUTCTime, getCurrentTime)

import Graphics
import SDLSession

import qualified Control.Wire as Netwire
import qualified Data.HashMap.Strict as HashMap
import qualified Graphics.UI.SDL as SDL
import qualified Graphics.UI.SDL.Keysym as Scancode

--------------------------------------------------------------------------------
type GameMonad = RWST (SDL.Renderer, SDLEvents) () (HashMap.HashMap String Image) IO

type Wire e a b = Netwire.Wire Session e GameMonad a b

type Session = Netwire.Timed Netwire.NominalDiffTime ()

data Game =
  forall exitCode.
  Game { gameWire :: forall a. Wire exitCode () Scene
       , gamePhysicsFramerate :: NominalDiffTime
       , gameQuit :: exitCode -> IO ()
       }

--------------------------------------------------------------------------------
simulate
  :: Netwire.NominalDiffTime -> Wire e () b -> (b -> IO ()) -> GameMonad e
simulate timeStep initialWire render = do
  -- Step once to obtain an initial frame
  (r, w, s) <- do
    events <- liftIO sampleEvents
    (ds, s) <- liftIO $ Netwire.stepSession session
    (r, w) <- local (_2 .~ events) $ Netwire.stepWire initialWire ds (Right ())
    return (r, w, s)

  now <- liftIO getCurrentTime
  go w s 0 now r

  where
    session = Netwire.countSession_ timeStep

    go wire' session' leftOver' previousTime lastOutput = do
      currentTime <- liftIO $ getCurrentTime
      let delta = leftOver' + currentTime `diffUTCTime` previousTime
          steps = floor (delta / timeStep) :: Int
          leftOver = delta - (fromIntegral steps * timeStep)

      (output, wire, session) <-
        stepNTimes lastOutput wire' session' (min 25 steps)

      either
        return
        (\x -> liftIO (render x) >> go wire session leftOver currentTime output)
        output

    stepNTimes r' w' s' steps =
      if steps <= 0
         then return (r', w', s')
          else do
            (ds, s) <- liftIO $ Netwire.stepSession s'
            (r, w) <- Netwire.stepWire w' ds (Right ())
            stepNTimes r w s (steps - 1)

--------------------------------------------------------------------------------
play :: Game -> IO ()
play Game {..} =
  SDL.withInit [SDL.InitEverything] $
  SDL.withWindow "Mario" (SDL.Position 0 0) (SDL.Size 800 600) [] $ \window ->
  SDL.withRenderer window SDL.FirstSupported [SDL.Accelerated] $ \renderer ->
  (fmap fst $ evalRWST (simulate gamePhysicsFramerate gameWire (render renderer)) (renderer, mempty) mempty)
    >>= gameQuit
  where
    render renderer scene = do
      drawScene renderer scene
      SDL.renderPresent renderer
