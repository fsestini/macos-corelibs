module MacSdk.Framework.Accessibility.Observer
  ( Observer
  , ObserverCallback
  , observerCreate
  , observerGetRunLoopSource
  -- * Notifications
  , addNotification
  , removeNotification
  ) where

import MacSdk.Framework.Types (PID)
import MacSdk.Framework.Accessibility.Error
import MacSdk.Framework.Accessibility.UIElement
import MacSdk.Framework.Accessibility.UINotification
import MacSdk.Framework.Accessibility.Types
import MacSdk.Framework.CoreFoundation

import Control.Monad.Managed
import Foreign hiding (with)
import Foreign.C.Types (CInt(..))
import Prelude hiding (String)

data AXObserver
type AXObserverRef = Ptr AXObserver
type Observer = Object AXObserver
instance CFClass AXObserver

foreign import ccall "wrapper"
  wrap_observer_callback :: AXObserverCallback -> IO (FunPtr AXObserverCallback)

foreign import ccall "Carbon/Carbon.h AXObserverCreate"
  ax_observer_create
  :: PID -> FunPtr AXObserverCallback -> Ptr AXObserverRef -> IO ForeignAXError

type AXObserverCallback =
  AXObserverRef -> AXUIElementRef -> CFStringRef -> StablePtr () -> IO ()

type ObserverCallback =
  Observer -> UIElement -> UINotification -> StablePtr () -> IO ()

toAXCallback :: ObserverCallback -> AXObserverCallback
toAXCallback f obs el str ptr = do
  obs' <- retainManageCFObj obs
  el'  <- retainManageCFObj el
  mNotif <- toNotif str
  maybe (pure ()) (\notif -> f obs' el' notif ptr) mNotif

observerCreate
  :: (MonadIO m, MonadError e m, AsAXError e)
  => PID -> ObserverCallback -> m Observer
observerCreate pid f =
  (liftIO (wrap_observer_callback (toAXCallback f)) >>= axObserverCreate pid)
    >>= liftIO . manageCFObj

axObserverCreate
  :: (MonadIO m, MonadError e m, AsAXError e)
  => PID -> FunPtr AXObserverCallback -> m AXObserverRef
axObserverCreate pid callb = except . liftIO $ alloca $ \p -> do
  err <- ax_observer_create pid callb p
  o <- peek p
  pure (either Left (const (Right o)) (toAXResult err))

foreign import ccall unsafe "Carbon/Carbon.h AXObserverGetRunLoopSource"
  axObserverGetRunLoopSource :: AXObserverRef -> IO CFRunLoopSourceRef

observerGetRunLoopSource :: Observer -> IO RunLoopSource
observerGetRunLoopSource obs = withCFPtr obs $ \optr ->
  axObserverGetRunLoopSource optr >>= retainManageCFObj

foreign import ccall unsafe "Carbon/Carbon.h AXObserverAddNotification"
  ax_observer_add_notification
  :: AXObserverRef -> AXUIElementRef -> CFStringRef -> StablePtr a
  -> IO ForeignAXError

addNotification
  :: (MonadIO m, MonadError e m, AsAXError e)
  => Observer -> UIElement -> UINotification -> StablePtr a -> m ()
addNotification obs el notif ptr = except . liftIO $ flip with pure $ do
  obs' <- managed $ withCFPtr obs
  el'  <- managed $ withCFPtr el
  liftIO $ notifString notif >>= \str ->
    fmap toAXResult (ax_observer_add_notification obs' el' str ptr)

foreign import ccall "Carbon/Carbon.h AXObserverRemoveNotification"
  ax_observer_remove_notification
  :: AXObserverRef -> AXUIElementRef -> CFStringRef -> IO ForeignAXError

removeNotification
  :: (MonadIO m, MonadError e m, AsAXError e)
  => Observer -> UIElement -> UINotification -> m ()
removeNotification obs el notif = except . liftIO . flip with pure $ do
  obs' <- managed $ withCFPtr obs
  el'  <- managed $ withCFPtr el
  liftIO $ notifString notif >>=
    fmap toAXResult . ax_observer_remove_notification obs' el'
