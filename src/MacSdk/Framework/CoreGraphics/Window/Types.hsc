{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE StandaloneDeriving #-}

module MacSdk.Framework.CoreGraphics.Window.Types where

#include <Carbon/Carbon.h>

import Data.Word
import Data.Bits
import MacSdk.Framework.Types (PID)
import MacSdk.Framework.CoreFoundation
import MacSdk.Framework.CoreGraphics.Rect
import System.IO.Unsafe (unsafePerformIO)

type CGWindowID = Word32
type CGWindowListOption = Word32
type WindowID = CGWindowID

nullWindowID :: WindowID
nullWindowID = #const kCGNullWindowID

-- | The data type used to specify the options for gathering a list of windows.
data WindowListOption
  = ExcludeDesktopElements
  -- ^ Exclude any windows from the list that are elements of the desktop,
  -- including the background picture and desktop icons.
  | OptionIncludingWindow
  -- ^ Include the specified window (from the second parameter in
  -- 'windowListCopyWindowInfo') in the returned list. You must combine this
  -- option with the 'OnScreenAboveWindow' or 'OnScreenBelowWindow' option to
  -- retrieve meaningful results.
  | OptionOnScreenAboveWindow
  -- ^ List all windows that are currently onscreen and in front of the window
  -- specified in the second parameter of 'windowListCopyWindowInfo'. Windows
  -- are returned in order from front to back.
  | OptionOnScreenBelowWindow
  -- ^ List all windows that are currently onscreen and in behind the window
  -- specified in the second parameter of 'windowListCopyWindowInfo'. Windows
  -- are returned in order from front to back.
  | OptionOnScreenOnly
  -- ^ List all windows that are currently onscreen. Windows are returned in
  -- order from front to back. When retrieving a list with this option, the
  -- second parameter of 'windowListCopyWindowInfo' should be set to 'Nothing'.

toCGWindowListOption :: WindowListOption -> CGWindowListOption
toCGWindowListOption = \case
  ExcludeDesktopElements -> (#const kCGWindowListExcludeDesktopElements)
  OptionIncludingWindow -> (#const kCGWindowListOptionIncludingWindow)
  OptionOnScreenAboveWindow -> (#const kCGWindowListOptionOnScreenAboveWindow)
  OptionOnScreenBelowWindow -> (#const kCGWindowListOptionOnScreenBelowWindow)
  OptionOnScreenOnly -> (#const kCGWindowListOptionOnScreenOnly)

orEdOptions :: [WindowListOption] -> CGWindowListOption
orEdOptions = foldr (\o cgo -> toCGWindowListOption o .|. cgo) 0

--------------------------------------------------------------------------------

foreign import ccall unsafe kCGWindowNumber_ :: IO CFStringRef
foreign import ccall unsafe kCGWindowStoreType_ :: IO CFStringRef
foreign import ccall unsafe kCGWindowLayer_ :: IO CFStringRef
foreign import ccall unsafe kCGWindowBounds_ :: IO CFStringRef
foreign import ccall unsafe kCGWindowSharingState_ :: IO CFStringRef
foreign import ccall unsafe kCGWindowAlpha_ :: IO CFStringRef
foreign import ccall unsafe kCGWindowOwnerPID_ :: IO CFStringRef
foreign import ccall unsafe kCGWindowMemoryUsage_ :: IO CFStringRef

foreign import ccall unsafe kCGWindowOwnerName_ :: IO CFStringRef
foreign import ccall unsafe kCGWindowName_ :: IO CFStringRef
foreign import ccall unsafe kCGWindowIsOnscreen_ :: IO CFStringRef
foreign import ccall unsafe kCGWindowBackingLocationVideoMemory_ :: IO CFStringRef

data WindowListKey :: * -> * where
  -- | ID of the window. The ID is unique within the current user session.
  WindowNumber :: WindowListKey WindowID
  WindowStoreType :: WindowListKey Number
  WindowLayer :: WindowListKey Number
  -- | The coordinates of the rectangle are specified in screen space, where the
  -- origin is in the upper-left corner of the main display.
  WindowBounds :: WindowListKey Rect
  WindowSharingState :: WindowListKey Number
  -- | The window’s alpha fade level. This number is in the range 0.0 to 1.0,
  -- where 0.0 is fully transparent and 1.0 is fully opaque.
  WindowAlpha :: WindowListKey Number
  WindowOwnerPID :: WindowListKey PID
  -- | An estimate of the amount of memory (measured in bytes) used by the
  -- window and its supporting data structures.
  WindowMemoryUsage :: WindowListKey Number

deriving instance Show (WindowListKey ty)

data OptionalWindowListKey :: * -> * where
  WindowOwnerName :: OptionalWindowListKey CFString
  WindowName :: OptionalWindowListKey CFString
  WindowIsOnscreen :: OptionalWindowListKey Bool
  WindowBackingLocationVideoMemory :: OptionalWindowListKey Bool

deriving instance Show (OptionalWindowListKey ty)

toWindowListKeyString :: WindowListKey a -> CFStringRef
toWindowListKeyString = unsafePerformIO . \case
  WindowNumber -> kCGWindowNumber_
  WindowStoreType -> kCGWindowStoreType_
  WindowLayer -> kCGWindowLayer_
  WindowBounds -> kCGWindowBounds_
  WindowSharingState -> kCGWindowSharingState_
  WindowAlpha -> kCGWindowAlpha_
  WindowOwnerPID -> kCGWindowOwnerPID_
  WindowMemoryUsage -> kCGWindowMemoryUsage_

toOptionalWindowListKeyString :: OptionalWindowListKey a -> CFStringRef
toOptionalWindowListKeyString = unsafePerformIO . \case
  WindowOwnerName -> kCGWindowOwnerName_
  WindowName -> kCGWindowName_
  WindowIsOnscreen -> kCGWindowIsOnscreen_
  WindowBackingLocationVideoMemory -> kCGWindowBackingLocationVideoMemory_
