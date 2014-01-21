{-# LANGUAGE Arrows #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module SDLSession
    ( SDLEvents
    , HasSDLEvents (..)
    , sampleEvents

      -- * Events
    , keyDown
    , keyUp

      -- * Intervals
    , keyHeld
    , keyMap
    ) where

import Prelude hiding ((.), id)
import Control.Applicative
import Control.Arrow ((&&&))
import Control.Category ((.), id)
import Control.Lens
import Control.Monad (liftM)
import Control.Monad.Loops (unfoldM)
import Control.Monad.Reader (MonadReader, ask)
import Data.Foldable (Foldable, asum)
import Data.Monoid (Monoid (..), (<>))
import Data.Traversable (Traversable)

import qualified Control.Wire as Netwire
import qualified Control.Wire.Unsafe.Event as Netwire
import qualified Data.Vector as Vector
import qualified Graphics.UI.SDL as SDL
import qualified Graphics.UI.SDL.Keysym as Scancode

--------------------------------------------------------------------------------
class HasSDLEvents t where
  sdlEvents :: Lens' t SDLEvents
  
instance HasSDLEvents SDLEvents where
  sdlEvents = id

--------------------------------------------------------------------------------
newtype SDLEvents = SDLEvents { getEvents :: Vector.Vector SDL.Event }
  deriving (Monoid)

sampleEvents :: IO SDLEvents
sampleEvents = (SDLEvents . Vector.fromList) <$> unfoldM SDL.pollEvent

--------------------------------------------------------------------------------
-- Various events into the SDL event system
eventData :: Lens' SDL.Event SDL.EventData
eventData f e = f (SDL.eventData e) <&> \dat -> e { SDL.eventData = dat }

keysym :: Traversal' SDL.EventData SDL.Keysym
keysym f (SDL.Keyboard m w r s) = SDL.Keyboard m w r <$> f s
keysym _ e = pure e

keyMovement :: Traversal' SDL.EventData SDL.KeyMovement
keyMovement f (SDL.Keyboard m w r s) =
  SDL.Keyboard <$> f m <*> pure w <*> pure r <*> pure s
keyMovement _ e = pure e

scancode :: Lens' SDL.Keysym SDL.Scancode
scancode f k = f (Scancode.keyScancode k) <&> \s -> k { Scancode.keyScancode = s }

--------------------------------------------------------------------------------
keyHeld
  :: (HasSDLEvents t, MonadReader t m, Monoid e, Monoid s)
  => Scancode.Scancode -> Netwire.Wire s e m a a
keyHeld s = proc a -> do
  down <- keyDown s -< ()
  up <- keyUp s -< ()
  Netwire.between -< (a, down, up)

--------------------------------------------------------------------------------
keyDown
  :: (MonadReader t m, HasSDLEvents t,  Monoid s)
  => Scancode.Scancode -> Netwire.Wire s e m a (Netwire.Event a)
keyDown s = keyEvent s [ has $ keyMovement.only SDL.KeyDown ]

keyUp
  :: (MonadReader t m, HasSDLEvents t, Monoid s)
  => Scancode.Scancode -> Netwire.Wire s e m a (Netwire.Event a)
keyUp s = keyEvent s [ has $ keyMovement.only SDL.KeyUp ]

keyEvent
  :: (MonadReader t m, HasSDLEvents t, Monoid s)
  => Scancode.Scancode -> [SDL.EventData -> Bool]
  -> Netwire.Wire s e m a (Netwire.Event a)
keyEvent s conditions = Netwire.mkGen_ $ \a -> do
  occuring <- liftM (hasEvent (has (keysym.scancode.only s) : conditions)) ask
  return (Right $ if occuring then Netwire.Event a else Netwire.NoEvent)

hasEvent :: (HasSDLEvents t) => [SDL.EventData -> Bool] -> t -> Bool
hasEvent conditions =
  has (sdlEvents . to getEvents . folded . eventData . filtered (and <$> sequence conditions))


--------------------------------------------------------------------------------
keyMap
  :: (Functor f, Foldable f, Monoid e, HasSDLEvents t, MonadReader t m, Monoid s)
  => f (Scancode.Scancode, b) -> Netwire.Wire s e m a b
keyMap = asum . fmap (\(k, b) -> pure b . keyHeld k)
