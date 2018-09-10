{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}

module MacSdk.Framework.CoreGraphics.Display
  ( DisplayID
  , DisplayChange(..)
  , DisplayReconfigurationCallback
  , DisplayCallbackToken
  , displayUUID
  , uuidString'
  , displayBounds
  , displayCount
  , activeDisplays
  , activeDisplay
  , setDisplayCallback
  , removeDisplayCallback
  , displayForWindow'
  ) where

import MacSdk.Framework.CoreFoundation
import MacSdk.Framework.CoreGraphics.Error
import MacSdk.Framework.CoreGraphics.Rect
import MacSdk.Framework.CoreGraphics.Window.Types
-- import MacSdk.Framework.Types (WindowID)

import Data.Word
import Data.Bits
import Control.Monad
import Prelude hiding (String)
import Control.Monad.Managed
import Foreign.C.Types (CUInt(..), CInt(..))
import Foreign.Ptr
import Foreign.Marshal.Array
import Foreign hiding (with)

type DisplayID = Word32

foreign import ccall unsafe "CGDisplayCreateUUIDFromDisplayID"
  cgCreateUUIDFromDisplayID :: DisplayID -> IO CFUUIDRef

displayUUID :: DisplayID -> IO UUID
displayUUID = cgCreateUUIDFromDisplayID >=> manageCFObj

foreign import ccall unsafe "CFUUIDCreateString"
  cfUUIDCreateString :: CFAllocatorRef -> CFUUIDRef -> IO CFStringRef

uuidString :: Allocator -> UUID -> IO CFString
uuidString al uuid = flip with pure $ do
  al' <- managed $ withCFPtr al
  uuid' <- managed $ withCFPtr uuid
  liftIO $ cfUUIDCreateString al' uuid' >>= manageCFObj

uuidString' :: UUID -> IO CFString
uuidString' uuid = nullAllocator >>= flip uuidString uuid

foreign import ccall unsafe cgDisplayBounds_ :: CUInt -> Ptr Rect -> IO ()

displayBounds :: DisplayID -> IO Rect
displayBounds dId = alloca $ \p -> do
  cgDisplayBounds_ (fromIntegral dId) p
  peek p

foreign import ccall unsafe "CGGetActiveDisplayList"
  cgGetActiveDisplayList
  :: CUInt -> Ptr DisplayID -> Ptr CUInt -> IO ForeignCGError

displayCount :: (MonadIO m, MonadError e m, AsCGError e) => m Int
displayCount = except . liftIO . alloca $ \p -> do
  err <- cgGetActiveDisplayList 0 nullPtr p
  let res = intToCGError err
  either (pure . Left) (const (Right <$> (fmap fromIntegral $ peek p))) res

activeDisplays :: (MonadIO m, MonadError e m, AsCGError e) => Int -> m [DisplayID]
activeDisplays maxIds = except . liftIO . allocaArray maxIds $ \p -> alloca $ \q -> do
  err <- cgGetActiveDisplayList (fromIntegral maxIds) p q
  let res = intToCGError err
  either (pure . Left)
    (const (fromIntegral <$> peek q >>= \count ->
               Right <$> peekArray count p)) res

--------------------------------------------------------------------------------

type DisplayChangeSummaryFlags = Word32

beginConfigFlag, movedFlag, setMainFlag, setModeFlag, addFlag, removeFlag,
  enabledFlag, disabledFlag, mirrorFlag, unmirrorFlag,
    desktopShapeChangedFlag :: DisplayChangeSummaryFlags
beginConfigFlag = 1
movedFlag = 1 `shiftL` 1
setMainFlag = 1 `shiftL` 2
setModeFlag = 1 `shiftL` 3
addFlag = 1 `shiftL` 4
removeFlag = 1 `shiftL` 5
enabledFlag = 1 `shiftL` 8
disabledFlag = 1 `shiftL` 9
mirrorFlag = 1 `shiftL` 10
unmirrorFlag = 1 `shiftL` 11
desktopShapeChangedFlag = 1 `shiftL` 12

data DisplayChange
  = DisplayBeginConfig
  | DisplayMoved
  | DisplaySetMain
  | DisplaySetMode
  | DisplayAdd
  | DisplayRemove
  | DisplayEnabled
  | DisplayDisabled
  | DisplayMirror
  | DisplayUnmirror
  | DisplayDesktopShapeChanged
  deriving (Eq)

toDisplayChange :: DisplayChangeSummaryFlags -> Maybe DisplayChange
toDisplayChange fl
  | Foreign.toBool (fl .&. beginConfigFlag) = Just DisplayBeginConfig
  | Foreign.toBool (fl .&. movedFlag) = Just DisplayMoved
  | Foreign.toBool (fl .&. setMainFlag) = Just DisplaySetMain
  | Foreign.toBool (fl .&. setModeFlag) = Just DisplaySetMode
  | Foreign.toBool (fl .&. addFlag) = Just DisplayAdd
  | Foreign.toBool (fl .&. removeFlag) = Just DisplayRemove
  | Foreign.toBool (fl .&. enabledFlag) = Just DisplayEnabled
  | Foreign.toBool (fl .&. disabledFlag) = Just DisplayDisabled
  | Foreign.toBool (fl .&. mirrorFlag) = Just DisplayMirror
  | Foreign.toBool (fl .&. unmirrorFlag) = Just DisplayUnmirror
  | Foreign.toBool (fl .&. desktopShapeChangedFlag) = Just DisplayDesktopShapeChanged
  | otherwise = Nothing

--------------------------------------------------------------------------------

type CGDisplayReconfigurationCallback =
  DisplayID -> DisplayChangeSummaryFlags -> Ptr () -> IO ()

foreign import ccall unsafe "wrapper" wrap_display_callback
  :: CGDisplayReconfigurationCallback -> IO (FunPtr CGDisplayReconfigurationCallback)

type DisplayReconfigurationCallback = DisplayID -> DisplayChange -> IO ()

toCGCallback :: DisplayReconfigurationCallback -> CGDisplayReconfigurationCallback
toCGCallback cb did flags _ = maybe (pure ()) (cb did) (toDisplayChange flags)

foreign import ccall "CGDisplayRegisterReconfigurationCallback"
  cgDisplayRegisterReconfigurationCallback_
  :: FunPtr CGDisplayReconfigurationCallback -> Ptr () -> IO ForeignCGError

foreign import ccall "CGDisplayRemoveReconfigurationCallback"
  cgDisplayRemoveReconfigurationCallback
  :: FunPtr CGDisplayReconfigurationCallback -> Ptr () -> IO ForeignCGError

newtype DisplayCallbackToken =
  DisplayCallbackToken (FunPtr CGDisplayReconfigurationCallback)

setDisplayCallback
  :: (MonadIO m, MonadError e m, AsCGError e) => DisplayReconfigurationCallback
  -> m DisplayCallbackToken
setDisplayCallback f = do
  fp <- liftIO $ wrap_display_callback (toCGCallback f)
  err <- liftIO $
    intToCGError <$> cgDisplayRegisterReconfigurationCallback_ fp nullPtr
  except (pure (err >> pure (DisplayCallbackToken fp)))

removeDisplayCallback
  :: (MonadIO m, MonadError e m, AsCGError e) => DisplayCallbackToken -> m ()
removeDisplayCallback (DisplayCallbackToken fp) =
  except . liftIO . fmap intToCGError $
    cgDisplayRemoveReconfigurationCallback fp nullPtr

-- foreign import ccall unsafe "CGSCopyManagedDisplayForWindow"
--   cgSCopyManagedDisplayForWindow_
--   :: ConnectionID -> CUInt -> IO CFStringRef

-- displayForWindow :: ConnectionID -> WindowID -> IO String
-- displayForWindow cid wid =
--   cgSCopyManagedDisplayForWindow_ cid (fromIntegral wid) >>= manageCFObj

--------------------------------------------------------------------------------

foreign import ccall unsafe display_for_window :: WindowID -> IO CFStringRef

displayForWindow' :: WindowID -> IO CFString
displayForWindow' = display_for_window >=> manageCFObj

foreign import ccall unsafe activeDisplay :: IO DisplayID
