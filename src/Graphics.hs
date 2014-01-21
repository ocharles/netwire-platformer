{-# LANGUAGE FlexibleContexts #-}
module Graphics
    ( Scene, drawScene, translate
    , Image, loadImage, imageRegion
    , clear
    , HasSDLRenderer (..)
    ) where

import Control.Applicative
import Control.Lens
import Control.Monad (liftM)
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.RWS.Class (MonadRWS)
import Data.Monoid (Monoid (..))
import Data.Semigroup (Semigroup (..))
import Data.Word (Word8)

import Linear
import Linear.Affine

import qualified Control.Wire as Netwire
import qualified Data.HashMap.Strict as HashMap
import qualified Graphics.UI.SDL as SDL

--------------------------------------------------------------------------------
data Scene = Scene { sceneRender :: SDL.Renderer -> Linear.V2 Int -> IO ()
                   }

instance Semigroup Scene where
  (Scene a) <> (Scene b) = Scene (\r o -> a r o >> b r o)

instance Monoid Scene where
  mempty = Scene (\_ _ -> return ())
  mappend = (<>)

drawScene :: SDL.Renderer -> Scene -> IO ()
drawScene renderer (Scene f) = f renderer 0

--------------------------------------------------------------------------------
translate :: Scene -> V2 Int -> Scene
translate (Scene f) translation =
  Scene $ \renderer offset -> f renderer (offset + translation)

--------------------------------------------------------------------------------
class HasSDLRenderer t where
  sdlRenderer :: Lens' t SDL.Renderer

--------------------------------------------------------------------------------
data Image = Image SDL.Texture

loadImage
  :: (HasSDLRenderer r, MonadRWS r w (HashMap.HashMap FilePath Image) m, MonadIO m)
  => FilePath -> Word8 -> Word8 -> Word8
  -> Netwire.Wire s e m a Image
loadImage fileName kR kG kB = Netwire.mkGen_ $ \_ -> do
  liftM Right $ cacheLookup (at fileName) $ do
    surface <- liftIO $ do
      surface <- SDL.loadBMP fileName
      surfaceFormat <- SDL.surfaceFormat surface
      key <- SDL.mapRGBA surfaceFormat kR kG kB 255
      SDL.setColorKey surface True key
      return surface

    renderer <- view sdlRenderer
    liftM Image $ liftIO (SDL.createTextureFromSurface renderer surface)

  where
    cacheLookup cacheKey action =
      use (cloneIndexedLens cacheKey) >>=
        maybe ((cloneIndexedLens cacheKey <?=) =<< action) return

--------------------------------------------------------------------------------
imageRegion :: Image -> SDL.Rect -> Point V2 Int -> [SDL.Flip] -> Scene
imageRegion (Image texture) bounds pos flip = Scene $ \renderer offset ->
  let position = pos .+^ offset
  in SDL.renderCopyEx renderer texture
                      (Just bounds)
                      (Just $ bounds & rectLoc .~ position)
                      0 Nothing
                      flip

--------------------------------------------------------------------------------
clear :: Word8 -> Word8 -> Word8 -> Word8 -> Scene
clear r g b a = Scene $ \renderer _ -> do
  SDL.setRenderDrawColor renderer r g b a
  SDL.renderClear renderer

--------------------------------------------------------------------------------
rectLoc :: Functor f => (Point V2 Int -> f (Point V2 Int)) -> SDL.Rect -> f SDL.Rect
rectLoc f (SDL.Rect x y w h) =
  f (P (V2 x y)) <&> \(P (V2 x' y')) -> SDL.Rect x' y' w h
