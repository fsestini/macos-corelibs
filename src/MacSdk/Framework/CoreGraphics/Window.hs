{-# LANGUAGE GADTs #-}

-- | Quartz Window Services
--
-- This includes the onscreen windows seen on the user’s desktop and any
-- offscreen windows used by the running applications. You can also use Quartz
-- Window Services to generate images based on the contents of windows.
--
-- See: https://developer.apple.com/documentation/coregraphics/quartz_window_services

module MacSdk.Framework.CoreGraphics.Window
  ( WindowID
  , WindowListOption(..)
  , WindowInfoDictionary
  , WindowListKey(..)
  , OptionalWindowListKey(..)
  , getDictionary
  -- * Getting window information
  , windowListCopyWindowInfo
  , windowInfoLookupKey
  , windowInfoLookupOptionalKey
  , foo
  ) where

import MacSdk.Framework.CoreFoundation
import MacSdk.Framework.CoreGraphics.Window.Types
import MacSdk.Framework.CoreGraphics.Rect
import Foreign (nullPtr)
import Control.Monad.IO.Class (MonadIO(..))
import Data.Maybe (fromMaybe)
import Control.Monad

--------------------------------------------------------------------------------
-- Getting window information

newtype WindowInfoDictionary = WindowInfoDictionary
  { getDictionary :: Dictionary
  }

-- CFArrayRef CGWindowListCopyWindowInfo(CGWindowListOption option, CGWindowID relativeToWindow);
foreign import ccall unsafe "CGWindowListCopyWindowInfo"
  cg_window_list_copy_window_info :: CGWindowListOption -> CGWindowID -> IO CFArrayRef

-- | Returns an array of 'Dictionary' elements, each of which contains
-- information about one of the windows in the current user session.
--
-- The second parameter specifies the ID of the window to use as a reference
-- point when determining which other window dictionaries to return. For options
-- that do not require a reference window, this parameter should be 'Nothing'.
--
-- The first parameter is a list of options describing which window dictionaries
-- to return. An empty list retrieves all windows, including both onscreen and
-- offscreen windows. When retrieving a list with this option, the second
-- parameter should be set to 'Nothing'.
--
-- If there are no windows matching the desired criteria, the function
-- returns an empty array. If you call this function from outside of a GUI
-- security session or when no window server is running, this function returns
-- 'Nothing'.
windowListCopyWindowInfo
  :: MonadIO m
  => [WindowListOption]
  -> Maybe WindowID
  -> m (Maybe (Array WindowInfoDictionary))
windowListCopyWindowInfo opts mwid = fmap (fmap Array) . liftIO $ do
  arr <- cg_window_list_copy_window_info (orEdOptions opts)
           (fromMaybe nullWindowID mwid)
  if arr == nullPtr then pure Nothing else fmap Just (retainManageCFObj arr)

windowInfoLookupKey :: MonadIO m => WindowInfoDictionary -> WindowListKey ty -> m ty
windowInfoLookupKey (WindowInfoDictionary dict) key = case key of
  WindowNumber -> do
    int <- getDictValue' dict skey >>= flip numberGetValue NumberSInt32Type
    pure (maybe (err key) fromIntegral int)
  WindowStoreType -> undefined
  WindowLayer -> undefined
  
  WindowBounds -> do
    rectDict <- getDictValue' dict skey
    fromMaybe (err key) <$> rectMakeWithDictionaryRepresentation rectDict
    
  WindowSharingState -> undefined
  WindowAlpha -> undefined
  WindowOwnerPID -> do
    int <- getDictValue' dict skey >>= flip numberGetValue NumberIntType
    pure (maybe (err key) fromIntegral int)

  WindowMemoryUsage -> undefined
  where skey = toWindowListKeyString key
        err :: WindowListKey ty -> ty
        err k = error ("windowInfoLookupKey: failed to lookup a required window list key: " ++ show k)

windowInfoLookupOptionalKey
  :: MonadIO m
  => WindowInfoDictionary -> OptionalWindowListKey ty -> m (Maybe ty)
windowInfoLookupOptionalKey (WindowInfoDictionary dict) key = case key of
  WindowOwnerName -> getDictValueSafe dict skey
  WindowName -> getDictValueSafe dict skey
  WindowIsOnscreen -> undefined
  WindowBackingLocationVideoMemory -> undefined
  where skey = toOptionalWindowListKeyString key

foo :: WindowInfoDictionary -> IO ()
foo d = do
  windowInfoLookupKey d WindowNumber >>= print
  windowInfoLookupOptionalKey d WindowName >>=
    maybe (pure ()) (toString CFStringEncodingUTF8 >=> print)
  windowInfoLookupOptionalKey d WindowOwnerName >>=
    maybe (pure ()) (toString CFStringEncodingUTF8 >=> print)
  putStrLn ""
