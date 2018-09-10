module MacSdk.Framework.Accessibility.Action where

import MacSdk.Framework.Accessibility.Error
import MacSdk.Framework.Accessibility.UIElement
import MacSdk.Framework.Accessibility.Types
import MacSdk.Framework.CoreFoundation

import Foreign hiding (with)
import Foreign.C.Types (CInt(..))

import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Managed

foreign import ccall unsafe "Carbon/Carbon.h AXUIElementPerformAction"
  ax_ui_element_perform_action
  :: AXUIElementRef -> CFStringRef -> IO ForeignAXError

foreign import ccall unsafe ax_press_action :: IO CFStringRef

-- actionString :: AXAction -> IO CFStringRef
-- actionString AXPressAction = ax_press_action

-- axUIElementPerformAction :: AXUIElementRef -> AXAction -> IO AXResult
-- axUIElementPerformAction ref =
--   actionString >=> fmap toAXResult . ax_ui_element_perform_action ref

-- axUIElementPerformAction'
--   :: MonadError' AXError m => AXUIElementRef -> AXAction -> m ()
-- axUIElementPerformAction' ref = resultAction . axUIElementPerformAction ref

-- performAction :: (MonadIO m, MonadError' AXError m) => UIElement -> AXAction -> m ()
-- performAction el =
--   except . liftIO . withCFPtr el .
--   flip (\ref -> actionString >=> fmap toAXResult . ax_ui_element_perform_action ref)

performAction
  :: (MonadIO m, MonadError e m, AsAXError e) => UIElement -> CFStringRef -> m ()
performAction el str = except . liftIO . withCFPtr el $ \p ->
  fmap toAXResult (ax_ui_element_perform_action p str)

-- | Performs the action of pressing a button.
actionPress :: (MonadIO m, MonadError e m, AsAXError e) => UIElement -> m ()
actionPress b = liftIO ax_press_action >>= performAction b

foreign import ccall unsafe "AXUIElementCopyActionNames"
  axUIElementCopyActionNames
  :: AXUIElementRef -> Ptr CFArrayRef -> IO ForeignAXError

-- | Returns a list of all the actions the specified accessibility object can
-- perform.
copyActionNames
  :: (MonadIO m, MonadError e m, AsAXError e) => UIElement -> m (Array CFString)
copyActionNames el = fmap Array . except . liftIO . flip with pure $ do
  elptr <- managed (withCFPtr el)
  arrptr <- managed alloca
  liftIO $ do
    err <- axUIElementCopyActionNames elptr arrptr
    either (pure . Left) (const (peek arrptr >>= fmap Right . manageCFObj))
      (toAXResult err)

