module MacSdk.Framework.Accessibility.Observer
  ( Observer
  , ObserverCallback
  , observerCreate
  , observerRelease
  , observerStart
  , observerStart'
  , observerStop
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
import Control.Monad ((>=>))
import Foreign hiding (with)
import Foreign.C.Types (CInt(..))
import Prelude hiding (String)
import Data.IORef

data AXObserver
type AXObserverRef = Ptr AXObserver
data Observer = Observer
  { _obsObj :: Object AXObserver
  , _obsRef :: Ref
  , _obsCallb :: FunPtr AXObserverCallback
  , _obsElems :: IORef [(UINotification, Ref)]
  }
instance CFClass AXObserver

foreign import ccall "wrapper"
  wrap_observer_callback :: AXObserverCallback -> IO (FunPtr AXObserverCallback)

foreign import ccall "Carbon/Carbon.h AXObserverCreate"
  ax_observer_create
  :: PID -> FunPtr AXObserverCallback -> Ptr AXObserverRef -> IO ForeignAXError

type AXObserverCallback =
  AXObserverRef -> AXUIElementRef -> CFStringRef -> StablePtr () -> IO ()

type ObserverCallback =
  -- Observer ->
  UIElement -> UINotification -> StablePtr () -> IO ()

toAXCallback :: ObserverCallback -> AXObserverCallback
toAXCallback f _ el str ptr = do
  el'  <- retainManageCFObj el
  mNotif <- toNotif str
  maybe (pure ()) (\notif -> f el' notif ptr) mNotif

-- | Creates an observer for the application with specified PID.
--
-- Note that this does not start the observer. Refer to 'observerStart' for
-- this.
--
-- Observers that are not longer useful must be released with 'observerRelease'
-- to avoid memory leaks.
observerCreate
  :: (MonadIO m, MonadError e m, AsAXError e)
  => PID -> ObserverCallback -> m Observer
observerCreate pid f = do
  fptr <- liftIO (wrap_observer_callback (toAXCallback f))
  obs <- axObserverCreate pid fptr >>= liftIO . manageCFObj
  liftIO $ do
    oref <- objRetain obs
    elems <- newIORef []
    pure (Observer obs oref fptr elems)

-- | Starts an observer by adding it to the given event loop, according to the
-- specified 'RunLoopMode'.
--
-- See 'observerStart'' for a version of this using default parameters.
observerStart :: MonadIO m => RunLoop -> RunLoopMode -> Observer -> m ()
observerStart rl rlm obs = liftIO $ do
  rls <- observerGetRunLoopSource obs
  b <- runLoopContainsSource rl rls rlm
  if b then pure () else runLoopAddSource rl rls rlm

-- | Starts an observer by adding it to the main run loop, using the default
-- mode.
observerStart' :: MonadIO m => Observer -> m ()
observerStart' o = liftIO mainRunLoop >>= \x -> observerStart x DefaultMode o

-- | Stops an observer, preventing it from receiving further notifications.
observerStop :: Observer -> IO ()
observerStop obs = observerGetRunLoopSource obs >>= runLoopSourceInvalidate

-- | Releases all resources associated to an observer.
observerRelease :: MonadIO m => Observer -> m ()
observerRelease (Observer _ ref fptr elems) = liftIO $ do
  refRelease ref
  freeHaskellFunPtr fptr
  readIORef elems >>= mapM_ refRelease . fmap snd

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
observerGetRunLoopSource obs = withCFPtr (_obsObj obs) $
  axObserverGetRunLoopSource >=> retainManageCFObj

foreign import ccall unsafe "Carbon/Carbon.h AXObserverAddNotification"
  ax_observer_add_notification
  :: AXObserverRef -> AXUIElementRef -> CFStringRef -> StablePtr a
  -> IO ForeignAXError

-- | Registers the specified observer to receive notifications from the
-- specified accessibility object.
addNotification
  :: (MonadIO m, MonadError e m, AsAXError e)
  => Observer -> UIElement -> UINotification -> StablePtr a -> m ()
addNotification obs el notif ptr = except . liftIO $ flip with pure $ do
  obs' <- managed $ withCFPtr (_obsObj obs)
  el'  <- managed $ withCFPtr el
  liftIO $ do
    str <- notifString notif
    ref <- objRetain el
    fmap toAXResult (ax_observer_add_notification obs' el' str ptr)
      <* atomicModifyIORef' (_obsElems obs) ((,()) . ((notif, ref) :))

foreign import ccall "Carbon/Carbon.h AXObserverRemoveNotification"
  ax_observer_remove_notification
  :: AXObserverRef -> AXUIElementRef -> CFStringRef -> IO ForeignAXError

lookupAndDelete :: Eq a => a -> [(a, b)] -> ([(a, b)], Maybe b)
lookupAndDelete x xs =
  maybe (xs, Nothing) ((filter ((/= x) . fst) xs,) . Just) (lookup x xs)

-- | Removes the specified notification from the list of notifications the
-- observer wants to receive from the accessibility object.
removeNotification
  :: (MonadIO m, MonadError e m, AsAXError e)
  => Observer -> UIElement -> UINotification -> m ()
removeNotification obs el notif = except . liftIO . flip with pure $ do
  obs' <- managed $ withCFPtr (_obsObj obs)
  el'  <- managed $ withCFPtr el
  liftIO $ do
    str <- notifString notif
    fmap toAXResult (ax_observer_remove_notification obs' el' str)
      <* (atomicModifyIORef' (_obsElems obs) (lookupAndDelete notif)
           >>= maybe (pure ()) refRelease)
