{-# LANGUAGE Arrows #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE RankNTypes #-}
import Prelude hiding ((.), id, until, mapM)

import Data.Either (lefts)
import Control.Lens
import Control.Monad (guard, liftM, msum, void)
import Control.Monad.Fix (MonadFix)
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Loops (unfoldM)
import Control.Monad.Reader.Class (MonadReader, ask)
import Control.Monad.State.Class (MonadState, get, gets, modify)
import Control.Monad.Trans.Maybe (runMaybeT)
import Control.Monad.Trans.Reader (runReaderT, ReaderT, withReaderT)
import Control.Monad.Trans.State (evalStateT, StateT)
import Control.Wire
import Data.Array (Array, (!))
import Data.Foldable (Foldable, asum, foldMap, minimumBy, toList)
import Data.List (sortBy)
import Data.Maybe (fromMaybe)
import Data.Ord (comparing)
import Data.Traversable (mapM, sequenceA)
import Data.Time (getCurrentTime, diffUTCTime)
import FRP.Netwire
import Linear
import Linear.Affine

import qualified Codec.Picture as JP
import qualified Codec.Picture.Types as JP
import qualified Control.Lens as Lens
import qualified Control.Monad as Monad
import qualified Control.Wire.Unsafe.Event as Unsafe
import qualified Data.Array as Array
import qualified Data.Foldable as Foldable
import qualified Data.Map as Map
import qualified Graphics.UI.SDL as SDL
import qualified Graphics.UI.SDL.Keysym as Scancode

import Debug.Trace
traceId x = traceShow x x

--------------------------------------------------------------------------------
data SceneElement = SceneElement (SDL.Renderer -> V2 Int -> IO ())

instance Semigroup SceneElement where
  (SceneElement a) <> (SceneElement b) = SceneElement (\r o -> a r o >> b r o)

instance Monoid SceneElement where
  mempty = SceneElement (\_ _ -> return ())
  mappend = (<>)

data MarioInput = MarioInput
  { marioJumping :: Event ()
  , marioXVelocity :: Double
  }

translate :: V2 Int -> SceneElement -> SceneElement
translate offset (SceneElement f) =
  SceneElement $ \r p -> f r (p & _xy +~ offset)

--------------------------------------------------------------------------------
gravity :: V2 Double
gravity = V2 0 800

terminalSpeed, jumpSpeed :: Num a => a
terminalSpeed = 345
jumpSpeed = 400

tileSize, playerSize :: (Fractional a, Num a) => V2 a
tileSize = V2 8 8
playerSize = V2 8 14

--------------------------------------------------------------------------------
correctVelocity
  :: (MonadReader (V2 (Maybe Double)) m, MonadState (V2 Double) m)
  => m (V2 Double)
correctVelocity = do
  ask >>= modify . flip (liftI2 fromMaybe)
  _y %= min terminalSpeed
  get

--------------------------------------------------------------------------------
correctPosition
  :: ( MonadReader (Array (Point V2 Int) Bool) m
     , MonadState (Point V2 Double) m)
  => m (Point V2 Double, V2 Double)
correctPosition = do
  world <- ask

  collisions <- unfoldM $ runMaybeT $ do
    currentPosition <- get

    let d = qd currentPosition
        adjacencies = take 3 $
          sortBy (comparing d) $
            filter ((< quadrance (tileSize * 3)) . d) $
              map ((* 16) . fmap fromIntegral) $
                filter (world !) $
                  Array.indices world

    msum $ map ?? adjacencies $ \tileCenter -> do
      let delta = currentPosition .-. tileCenter
          gap = fmap abs delta - (tileSize + playerSize)

          correct axis =
            let onAxis = gap * axis
                direction = if dot delta axis < 0 then -1 else 1
            in [ onAxis * direction | dot onAxis axis < 0 ]

      correction <- liftM (minimumBy (comparing quadrance))
                          (mapM correct basis)

      _xy -= correction
      return correction

  gets $ \relocated -> (relocated, Foldable.foldl' (+) 0 collisions)

--------------------------------------------------------------------------------
hitHead, onFloor :: (Num a, Ord a, Fractional a) => V2 a -> Bool
hitHead collisions = dot collisions (V2 0 1) < 0
onFloor collisions = dot collisions (V2 0 1) > 0.5

--------------------------------------------------------------------------------
mario
  :: ( Fractional t, HasTime t s, Monoid e, Monad m, MonadFix m
     , MonadState cache m, At cache, IxValue cache ~ SDL.Texture
     , MonadReader SDL.Renderer m, MonadIO m
     , Index cache ~ FilePath)
  => Wire s e m (MarioInput, Array (Point V2 Int) Bool) (SceneElement, Point V2 Double)
mario = proc (m, world) -> do
  rec
    collisions <- delay 0 -< collisions'

    let velocityCorrections =
          V2 (return (marioXVelocity m))
             (asum [ [ negate jumpSpeed | onFloor collisions
                                        , Unsafe.occurred (marioJumping m) ]
                   , [ 0 | hitHead collisions ]
                   , [ gravity ^. _y | onFloor collisions ]
                   ])

    velocity <-
      integralWith correctVelocity gravity -< (gravity, velocityCorrections)

    (position, collisions') <-
      integralWith correctPosition (P (V2 0 0)) -< (P velocity, world)

  rec
    lastFlip <- delay [] -< flip
    let flip = case signum (marioXVelocity m) of
                 0 -> lastFlip
                 1 -> []
                 -1 -> [SDL.Horizontal]

  yVelocity <- derivative <|> 0 -< position ^. _y

  frame <-
    case compare yVelocity 0 of
      LT -> pure (SDL.Rect 145 60 16 28) -< ()
      GT -> pure (SDL.Rect 168 60 16 28) -< ()
      EQ -> if abs (velocity ^. _x) > 0
              then cycleFrames runFrames -< ()
              else pure idleFrame        -< ()

  sceneElement <- tile "mario.bmp" (255, 128, 64) -< (position, frame, flip)

  returnA -< (sceneElement, position)

 where

    runFrames = cycle [ SDL.Rect 21 60 16 28, SDL.Rect 38 60 16 28 ]

    idleFrame = SDL.Rect 4 60 16 28

cycleFrames (a:as) = pure a . for (1 / 10) --> cycleFrames as

--------------------------------------------------------------------------------
{-tile-}
  {-:: (Monoid s, MonadIO m, MonadState cache m, At cache, IxValue cache ~ SDL.Texture, Index cache ~ FilePath, MonadReader SDL.Renderer m)-}
  {-=> FilePath -> Wire s e m (Point V2 Double, SDL.Rect, [SDL.Flip]) SceneElement-}
tile fileName (kR, kG, kB) = mkGen $ \d a -> do
  renderer <- ask

  texture <- cacheLookup (Lens.at fileName) $ liftIO $ do
    surface <- SDL.loadBMP fileName
    surfaceFormat <- SDL.surfaceFormat surface
    key <- SDL.mapRGBA surfaceFormat kR kG kB 255
    SDL.setColorKey surface True key
    SDL.createTextureFromSurface renderer surface

  let move =
        mkPureN $ \(P pos, bounds, flipSprite) ->
          let draw r offset = void $ liftIO $
                SDL.renderCopyEx
                  r texture
                  (Just bounds)
                  (Just $ SDL.Rect 0 0 (SDL.rectW bounds) (SDL.rectH bounds)
                  & rectLoc +~ fmap round pos
                  & rectLoc +~ offset
                  & rectLoc -~ ((round . (/ 2) . fromIntegral) <$> bounds ^. rectSize))
                  0 Nothing
                  flipSprite

          in (Right (SceneElement draw), move)
  stepWire move d (Right a)

 where

  cacheLookup cacheKey action =
    use (cloneIndexedLens cacheKey) >>=
      maybe ((cloneIndexedLens cacheKey <?=) =<< action) return

--------------------------------------------------------------------------------
main :: IO ()
main =
  SDL.withInit [SDL.InitEverything] $
  SDL.withWindow "Mario" (SDL.Position 0 0) (SDL.Size 800 600) [] $ \window ->
  SDL.withRenderer window SDL.FirstSupported [SDL.Accelerated] $ \renderer -> do

  Right (JP.ImageRGB8 level) <- JP.readImage "level.png"
  let worldBounds = (0, P (V2 (JP.imageWidth level - 1) (JP.imageHeight level - 1)))
      world = Array.listArray worldBounds $
                map (\(P (V2 x y)) -> JP.computeLuma (JP.pixelAt level x y) == 0)
                  (Array.range worldBounds)

  getCurrentTime >>=
    flip evalStateT Map.empty .
    flip runReaderT renderer .
      reactimate (return mempty)
                 (game world)
                 (countSession_ timestep) 0

 where

  timestep = 1 / 200

  game w = proc _ -> do
    input <- mapWire (withReaderT (view _1))
               (MarioInput
                  <$> keyDown Scancode.Space
                  <*> asum [ 75 . whileKeyHeld Scancode.Right
                           , -75 . whileKeyHeld Scancode.Left
                           , 0 ]) -< ()

    (mario, marioPosition) <- mapWire (withReaderT (view _2)) mario -< (input, w)

    let offset = V2 (max 0 (marioPosition ^. _x - 400)) 0

    world <-
      foldMap (\(coords, isSolid) -> mapWire (withReaderT (view _2)) $
        if isSolid
          then tile "level-tiles.bmp" (37, 65, 82) .
                 pure ( fromIntegral <$> coords * 16
                      , let emptyNeighbour offset =
                              let neighbour = coords & _xy +~ offset
                              in guard $ not $
                                  Array.inRange (Array.bounds w) neighbour && w ! neighbour
                            tileIndex = fromMaybe (V2 154 116) $ asum
                              [ V2 120 150 <$ do emptyNeighbour (V2 (-1) 0)
                                                 emptyNeighbour (V2 0 (-1))
                              , V2 154 150 <$ do emptyNeighbour (V2 1 0)
                                                 emptyNeighbour (V2 0 (-1))
                              , V2 120 184 <$ do emptyNeighbour (V2 (-1) 0)
                                                 emptyNeighbour (V2 0 1)
                              , V2 154 184 <$ do emptyNeighbour (V2 1 0)
                                                 emptyNeighbour (V2 0 1)
                              , V2 154 167 <$ emptyNeighbour (V2 1 0)
                              , V2 120 167 <$ emptyNeighbour (V2 (-1) 0)
                              , V2 137 150  <$ emptyNeighbour (V2 0 (-1))
                              , V2 137 184  <$ emptyNeighbour (V2 0 1)
                              ]
                            aTile = SDL.Rect 0 0 16 16
                        in rectLoc +~ tileIndex $ aTile
                      , [])
          else mempty)
        (Array.assocs w)
          -< ()

    coins <- mapWire (withReaderT (view _2)) (pSwitch coinRouter coins coinObserver coinSwitch) -< marioPosition

    let coinGraphics = Map.foldlWithKey'
                         (\scene (P pos) graphic -> scene <> translate pos graphic)
                         mempty
                         coins


    returnA -< translate (negate $ view _xy $ round <$> offset) (world <> coinGraphics <> mario)

  coins = Map.fromList [ (P (V2 (16 * (x + 20)) (16 * 23)), coin) | x <- [0..5] ]

  coinRouter marioPosition =
    Map.mapWithKey $ \pos wire ->
      if fmap round marioPosition >= pos .-^ 8 && fmap round marioPosition <= pos .+^ 8
         then (Right $ Unsafe.Event (), wire)
         else (Right $ Unsafe.NoEvent, wire)

  coinObserver = proc (marioPosition, coins) -> do
    let events = Map.elems $ flip Map.mapWithKey coins $ \pos _ ->
          if fmap round marioPosition >= pos .-^ 8 && fmap round marioPosition <= pos .+^ 8
            then Unsafe.Event (Map.delete pos)
            else mempty

    returnA -< mconcat events

  coinSwitch col f = pSwitch coinRouter (f col) coinObserver coinSwitch

  reactimate r' w' s' leftOver' previousTime = do
    currentTime <- liftIO getCurrentTime
    let delta = leftOver' + currentTime `diffUTCTime` previousTime
        steps = floor (delta / timestep) :: Int
        leftOver = delta - (fromIntegral steps * timestep)

    (r, w, s) <- stepNTimes r' w' s' (min 25 steps)

    case r of
      Left () -> return ()
      Right (SceneElement a) -> do
        renderer <- ask
        liftIO $ do
          SDL.setRenderDrawColor renderer 122 183 255 255
          SDL.renderClear renderer
          a renderer 0
          SDL.renderPresent renderer

        reactimate r w s leftOver currentTime

  stepNTimes r' w' s' steps =
    if steps <= 0
        then return (r', w', s')
        else do
          events <- liftIO (unfoldM SDL.pollEvent)
          (ds, s) <- liftIO (stepSession s')
          (r, w) <-  withReaderT (\r -> (events, r)) (stepWire w' ds (Right ()))
          stepNTimes r w s (steps - 1)

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
whileKeyHeld
  :: (Functor m, Foldable f, Monoid e, MonadReader (f SDL.Event) m)
  => Scancode.Scancode -> Wire s e m t t
whileKeyHeld s = proc a -> do
  down <- keyDown s -< ()
  up <- keyUp s -< ()
  between -< (a, down, up)

--------------------------------------------------------------------------------
keyDown
  :: (Monad m, Functor m, Foldable f, MonadReader (f SDL.Event) m)
  => Scancode.Scancode -> Wire s e m a (Event a)
keyDown s = keyEvent s [ has $ keyMovement.only SDL.KeyDown ]

keyUp
  :: (Monad m, Functor m, Foldable f, MonadReader (f SDL.Event) m)
  => Scancode.Scancode -> Wire s e m a (Event a)
keyUp s = keyEvent s [ has $ keyMovement.only SDL.KeyUp ]

keyEvent
  :: (Monad m, Functor m, Foldable f, MonadReader (f SDL.Event) m)
  => Scancode.Scancode -> [SDL.EventData -> Bool] -> Wire s e m a (Event a)
keyEvent s conditions = mkGen_ $ \a -> do
  occuring <- hasEvent (has (keysym.scancode.only s) : conditions)
  return . Right $ if occuring then Unsafe.Event a else Unsafe.NoEvent

hasEvent
  :: (Monad m, Functor m, Foldable f, MonadReader (f SDL.Event) m)
  => [SDL.EventData -> Bool] -> m Bool
hasEvent conditions =
  has (folded . eventData . filtered (and <$> sequence conditions)) <$> ask

--------------------------------------------------------------------------------
rectLoc :: Functor f => (V2 Int -> f (V2 Int)) -> SDL.Rect -> f SDL.Rect
rectLoc f (SDL.Rect x y w h) =
  f (V2 x y) <&> \(V2 x' y') -> SDL.Rect x' y' w h

rectSize :: Functor f => (V2 Int -> f (V2 Int)) -> SDL.Rect -> f SDL.Rect
rectSize f (SDL.Rect x y w h) =
  f (V2 w h) <&> \(V2 w' h') -> SDL.Rect x y w' h'


--------------------------------------------------------------------------------
coin = proc collected -> do
  frame <- cycleFrames (cycle [ SDL.Rect 0 0 16 16
                              , SDL.Rect 16 0 16 16
                              , SDL.Rect 32 0 16 16
                              , SDL.Rect 48 0 16 16
                              ]) -< ()

  tile "coin.bmp" (255, 0, 255) -< (0, frame, [])

  --until -< (graphic, collected)

--------------------------------------------------------------------------------
pSwitch
  :: (Monad m, Monoid e, Monoid s, Traversable f)
  => (forall w. a -> f w -> f (Either e b, w))
  -> f (Wire s e m b c)
  -> Wire s e m (a, f (Either e c)) (Event d)
  -> (f (Wire s e m b c) -> d -> Wire s e m a (f c))
  -> Wire s e m a (f c)
pSwitch route wires observer switch = mkGen $ \s a -> do
  stepped <- mapM (\(input, w) -> stepWire w s input) (route a wires)
  (observation, newObserver) <- stepWire observer s (Right (a, fmap fst stepped))

  case observation of
    Right Unsafe.NoEvent ->
      return (sequenceA (fmap fst stepped), pSwitch route (fmap snd stepped) newObserver switch)

    Right (Unsafe.Event d) ->
      return (sequenceA (fmap fst stepped), switch (fmap snd stepped) d)


--------------------------------------------------------------------------------
rpSwitch
  :: (Monad m, Monoid s, Traversable f)
  => (forall w. a -> f w -> f (Either e b, w))
  -> f (Wire s e m b c)
  -> Wire s e m (a, Event (f (Wire s e m b c) -> f (Wire s e m b c))) (f c)
rpSwitch route wires' = mkGen $ \s (a, e) -> do
  let wires = Unsafe.event id id e $ wires' 
  stepped <- mapM (\(input, w) -> stepWire w s input) (route a wires)

  return (sequenceA (fmap fst stepped), rpSwitch route wires)

--------------------------------------------------------------------------------
moduloCounter :: (Integral a) => a -> a -> Wire s e m a (a, Event a)
moduloCounter initialValue modulo = go initialValue
 where
  go n = mkSFN $ \x ->
    let (rollOver, n') = (n + x) `divMod` modulo
        rollOverEvent | rollOver > 0 = Unsafe.Event rollOver
                      | otherwise    = Unsafe.NoEvent
    in ((n', rollOverEvent), go n') 
