{-# LANGUAGE KindSignatures #-}

module MacSdk.Framework.Accessibility.Attribute
  ( Attribute(..)
  , setAttribute
  , attributeValue
  , isAttributeSettable
  , role
  , subrole
  , copyAttributeNames
  , Role(..)
  , Subrole(..)
  ) where

import Control.Monad.Managed
import Control.Monad.Except
import Foreign hiding (with)
import Foreign.C.Types (CInt(..), CBool(..))
import MacSdk.Framework.Accessibility.Error
import MacSdk.Framework.Accessibility.UIElement
import MacSdk.Framework.Accessibility.Types
import MacSdk.Framework.Accessibility.Role
import MacSdk.Framework.Accessibility.Attribute.Types
import MacSdk.Framework.CoreFoundation

--------------------------------------------------------------------------------

foreign import ccall "Carbon/Carbon.h AXUIElementSetAttributeValue"
  ax_ui_element_set_attribute_value
  :: AXUIElementRef -> CFStringRef -> CFTypeRef -> IO ForeignAXError

setAttribute
  :: (MonadIO m, MonadError e m, AsAXError e)
  => UIElement -> Attribute a -> a -> m ()
setAttribute el at ob = except . liftIO $ withCFPtr el $ \el' -> do
  str <- liftIO $ toAttributeString' at
  p <- fst (attrGetterSetter at) ob
  fmap toAXResult (ax_ui_element_set_attribute_value el' str p)

--------------------------------------------------------------------------------

foreign import ccall "Carbon/Carbon.h AXUIElementCopyAttributeValue"
  ax_ui_element_copy_attribute_value
  :: AXUIElementRef -> CFStringRef -> Ptr CFTypeRef -> IO ForeignAXError

-- | Retrieves the role of a UIElement.
--
-- @
-- role = flip attributeValue RoleAttribute
-- @
role :: (MonadIO m, MonadError e m, AsAXError e) => UIElement -> m Role
role = flip attributeValue RoleAttribute

-- | Retrieves the subrole of a UIElement.
--
-- @
-- subrole = flip attributeValue SubroleAttribute
-- @
subrole :: (MonadIO m, MonadError e m, AsAXError e) => UIElement -> m Subrole
subrole = flip attributeValue SubroleAttribute

attributeValue
  :: (MonadIO m, MonadError e m, AsAXError e)
  => UIElement -> Attribute a -> m a
attributeValue uiel attr = except . liftIO . flip with pure $ do
  el <- managed (withCFPtr uiel)
  p <- managed alloca
  liftIO $ do
    str <- toAttributeString' attr
    res <- ax_ui_element_copy_attribute_value el str p
    either (pure . Left)
           (const (peek p >>= fmap Right . snd (attrGetterSetter attr)))
           (toAXResult res)

safeAttributeValue
  :: (MonadError e m, AsAXError e, MonadIO m)
  => UIElement -> Attribute a -> m (Maybe a)
safeAttributeValue el attr = do
  eith <- runExceptT (attributeValue el attr)
  case eith of
    Left AXErrorNoValue -> pure Nothing
    Left e -> throwing _AXError e
    Right x -> pure (Just x)

--------------------------------------------------------------------------------

foreign import ccall unsafe "Carbon/Carbon.h AXUIElementIsAttributeSettable"
  ax_ui_element_is_attribute_settable
  :: AXUIElementRef -> CFStringRef -> Ptr CBool -> IO ForeignAXError

axUIElementIsAttributeSettable
  :: AXUIElementRef -> Attribute a -> IO (Either AXError CBool)
axUIElementIsAttributeSettable ref a = do
  str <- toAttributeString' a
  alloca $ \p -> do
    err <- poke p (Foreign.fromBool False) >>
      ax_ui_element_is_attribute_settable ref str p
    res <- peek p
    pure (either Left (const (Right res)) (toAXResult err))

isAttributeSettable
  :: (MonadIO m, MonadError e m, AsAXError e)
  => UIElement -> Attribute a -> m Bool
isAttributeSettable el =
  fmap Foreign.toBool . except . liftIO . withCFPtr el .
    flip axUIElementIsAttributeSettable

foreign import ccall unsafe "AXUIElementCopyAttributeNames"
  axUIElementCopyAttributeNames
  :: AXUIElementRef -> Ptr CFArrayRef -> IO ForeignAXError

-- | Returns a list of all the attributes supported by the specified
-- accessibility object.
copyAttributeNames
  :: (MonadIO m, MonadError e m, AsAXError e)
  => UIElement -> m (Array CFString)
copyAttributeNames el = fmap Array . except . liftIO . flip with pure $ do
  elptr <- managed (withCFPtr el)
  arrptr <- managed alloca
  liftIO $ do
    err <- axUIElementCopyAttributeNames elptr arrptr
    either (pure . Left) (const (peek arrptr >>= fmap Right . manageCFObj))
      (toAXResult err)
