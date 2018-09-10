module MacSdk.Framework.Accessibility.UIElement
  (
  -- * UI elements
    UIElement
  , createAppElement
  , getWindowElementID
  , setMessagingTimeout
  , systemWideUIElement
  ) where

import MacSdk.Framework.Accessibility.Error
import MacSdk.Framework.Accessibility.Types
import MacSdk.Framework.CoreFoundation.Object
import MacSdk.Framework.Types (PID)
import MacSdk.Framework.CoreGraphics.Window (WindowID)
import Control.Monad.IO.Class(MonadIO(..))

import Foreign
import Foreign.C.Types (CInt(..))
import Control.Monad
import Control.Arrow ((>>>))
import Data.Bifunctor (first)

foreign import ccall unsafe "AXUIElementCreateApplication"
  ax_ui_element_create_application :: PID -> IO AXUIElementRef

-- | Create a UIElement from a PID, representing the application associated to
-- it.
createAppElement :: MonadIO m => PID -> m UIElement
createAppElement =
  (ax_ui_element_create_application >=> manageCFObj) >>> liftIO

foreign import ccall unsafe "_AXUIElementGetWindow"
  axUIElementGetWindow :: AXUIElementRef -> Ptr WindowID -> IO ForeignAXError

-- | Retrieve the WindowID of the window associated to the UI element. The
-- function fail if, among other things, the specified UI element does not
-- correspond to a valid window.
getWindowElementID
  :: (MonadIO m, MonadError e m, AsAXError e)
  => UIElement -> m WindowID
getWindowElementID el =
  except . liftIO . withCFPtr el $ \elptr -> alloca $ \widptr -> do
    err <- axUIElementGetWindow elptr widptr
    -- AXUIElementGetWindow occasionally returns AXErrorIllegalArgument, but
    -- AXErrorInvalidUIElement seems more appropriate, since the first argument
    -- is the only one that could possibly be invalid.
    let res = first (\e -> case e of
                        { AXErrorIllegalArgument -> AXErrorInvalidUIElement
                        ; _ -> e }) $ toAXResult err
    either (pure . Left) (const (Right <$> peek widptr)) res

foreign import ccall unsafe "AXUIElementSetMessagingTimeout"
  axUIElementSetMessagingTimeout :: AXUIElementRef -> Float -> IO ForeignAXError

setMessagingTimeout
  :: (MonadIO m, MonadError e m, AsAXError e) => UIElement -> Float -> m ()
setMessagingTimeout el f = except . liftIO . fmap toAXResult $
  withCFPtr el (`axUIElementSetMessagingTimeout` f)

foreign import ccall unsafe "AXUIElementCreateSystemWide"
  axUIElementCreateSystemWide :: IO AXUIElementRef

-- | Returns the system-wide accessibility object, that provides access to
-- system attributes.
systemWideUIElement :: IO UIElement
systemWideUIElement = axUIElementCreateSystemWide >>= manageCFObj
