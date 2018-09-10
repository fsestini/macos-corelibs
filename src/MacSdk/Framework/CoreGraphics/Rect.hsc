{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RankNTypes #-}

#include <Carbon/Carbon.h>

{- | Storable rectangles. -}

module MacSdk.Framework.CoreGraphics.Rect where

import Foreign hiding (with)
import Foreign.C.Types (CBool(..))
import MacSdk.Framework.CoreFoundation
import MacSdk.Framework.Accessibility.Value
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Managed

type Lens' s a = forall f . Functor f => (a -> f a) -> s -> f s

lens :: (s -> a) -> (s -> a -> s) -> Lens' s a
lens getter setter f s = setter s <$> f (getter s)

data Point = Point { xCoord :: !Double, yCoord :: !Double } deriving (Eq, Show)
data Size = Size { width :: !Double, height :: !Double } deriving (Eq, Show)

-- | Lens focused on the origin of a rectangle.
originL :: Lens' Rect Point
originL = lens origin $ \r p -> case r of Rect _ s -> Rect p s

-- | Lens focused on the size of a rectangle.
sizeL :: Lens' Rect Size
sizeL = lens size $ \r s -> case r of Rect o _ -> Rect o s

-- | Lens focused on the @x@ coordinate of a rectangle.
xL :: Lens' Point Double
xL = lens xCoord $ \p x -> case p of Point _ y -> Point x y

-- | Lens focused on the @y@ coordinate of a rectangle.
yL :: Lens' Point Double
yL = lens yCoord $ \p y -> case p of Point x _ -> Point x y

-- | Lens focused on the width of a rectangle.
widthL :: Lens' Size Double
widthL = lens width $ \s w -> case s of Size _ h -> Size w h

-- | Lens focused on the height of a rectangle.
heightL :: Lens' Size Double
heightL = lens height $ \s h -> case s of Size w _ -> Size w h

-- | Type of rectangular shapes.
data Rect = Rect
  { origin :: Point
  -- ^ Origin of the rectangle, corresponding to the top-left corner.
  , size :: Size
  -- ^ Size of the rectangle.
  } deriving (Eq, Show)

instance Storable Point where
  sizeOf _ = #{size CGPoint}
  alignment _ = #{alignment CGPoint}
  peek ptr = do
    x' <- #{peek CGPoint, x} ptr
    y' <- #{peek CGPoint, y} ptr
    return $ Point x' y'
  poke ptr (Point x' y') = do
    #{poke CGPoint, x} ptr x'
    #{poke CGPoint, y} ptr y'

instance Storable Size where
  sizeOf _ = #{size CGSize}
  alignment _ = #{alignment CGSize}
  peek ptr = do
    width' <- #{peek CGSize, width} ptr
    height' <- #{peek CGSize, height} ptr
    return $ Size width' height'
  poke ptr (Size width' height') = do
    #{poke CGSize, width} ptr width'
    #{poke CGSize, height} ptr height'

instance Storable Rect where
  sizeOf _ = #{size CGRect}
  alignment _ = #{alignment CGRect}
  peek ptr = do
    origin' <- #{peek CGRect, origin} ptr
    size' <- #{peek CGRect, size} ptr
    return $ Rect origin' size'
  poke ptr (Rect origin' size') = do
    #{poke CGRect, origin} ptr origin'
    #{poke CGRect, size} ptr size'

foreign import ccall unsafe create_cfpoint :: Ptr Point -> IO CFTypeRef
foreign import ccall unsafe create_cfsize :: Ptr Size -> IO CFTypeRef

foreign import ccall unsafe
  ax_value_get_cgpoint :: AXValueRef -> Ptr Point -> IO CBool
foreign import ccall unsafe
  ax_value_get_cgsize  :: AXValueRef -> Ptr Size -> IO CBool

axValueGetPoint :: AXValueRef -> IO (Maybe Point)
axValueGetPoint ref = alloca $ \p -> do
  res <- ax_value_get_cgpoint ref p
  if Foreign.toBool res then Just <$> peek p else pure Nothing

valueGetPoint :: Value -> IO (Maybe Point)
valueGetPoint = flip withCFPtr axValueGetPoint

axValueGetSize :: AXValueRef -> IO (Maybe Size)
axValueGetSize ref = alloca $ \p -> do
  res <- ax_value_get_cgsize ref p
  if Foreign.toBool res then Just <$> peek p else pure Nothing

valueGetSize :: Value -> IO (Maybe Size)
valueGetSize = flip withCFPtr axValueGetSize

-- | Turn a point value into a Core Foundation object.
createCFPoint :: Point -> IO CFObject
createCFPoint pt = alloca $ \p ->
  poke p pt >> create_cfpoint p >>= manageCFObj

-- | Turn a size value into a Core Foundation object.
createCFSize :: Size -> IO CFObject
createCFSize sz = alloca $ \p ->
  poke p sz >> create_cfsize p >>= manageCFObj

foreign import ccall unsafe "CGRectMakeWithDictionaryRepresentation"
  cgRectMakeWithDictionaryRepresentation
  :: CFDictionaryRef -> Ptr Rect -> IO CBool

rectMakeWithDictionaryRepresentation :: MonadIO m => Dictionary -> m (Maybe Rect)
rectMakeWithDictionaryRepresentation dict = liftIO . flip with pure $ do
  d <- managed $ withCFPtr dict
  ptr <- managed alloca
  liftIO $ cgRectMakeWithDictionaryRepresentation d ptr >>= \b ->
    if toBool b then Just <$> peek ptr else pure Nothing
