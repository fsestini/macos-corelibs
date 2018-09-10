module MacSdk.Framework.Accessibility
  ( module MacSdk.Framework.Accessibility.Action
  , module MacSdk.Framework.Accessibility.Attribute
  , module MacSdk.Framework.Accessibility.Error
  , module MacSdk.Framework.Accessibility.Observer
  , module MacSdk.Framework.Accessibility.UIElement
  , module MacSdk.Framework.Accessibility.UINotification
  , module MacSdk.Framework.Accessibility.Value
  , trustedCheckOptionPrompt
  , isProcessTrustedWithOptions
  , checkAXPrivileges
  ) where

import MacSdk.Framework.Accessibility.Action
import MacSdk.Framework.Accessibility.Attribute
import MacSdk.Framework.Accessibility.Error (AXError(..), AsAXError(..))
import MacSdk.Framework.Accessibility.Observer
import MacSdk.Framework.Accessibility.UIElement
import MacSdk.Framework.Accessibility.UINotification
import MacSdk.Framework.Accessibility.Value

import MacSdk.Framework.CoreFoundation
import Foreign
import Foreign.C.Types (CBool(..))

foreign import ccall unsafe axTrustedCheckOptionPrompt :: IO CFStringRef

trustedCheckOptionPrompt :: IO CFString
trustedCheckOptionPrompt = axTrustedCheckOptionPrompt >>= manageCFObj

foreign import ccall unsafe "AXIsProcessTrustedWithOptions"
  axIsProcessTrustedWithOptions :: Ptr () -> IO CBool

isProcessTrustedWithOptions :: Dictionary -> IO Bool
isProcessTrustedWithOptions =
  fmap Foreign.toBool . flip withCFPtr (axIsProcessTrustedWithOptions . castPtr)

-- | Check whether the current process has been granted Accessibility privileges
-- by the operating system.
checkAXPrivileges :: IO Bool
checkAXPrivileges = do
  allo <- nullAllocator
  s <- trustedCheckOptionPrompt
  b <- booleanTrue
  createDictionary allo [s] [b] >>= isProcessTrustedWithOptions
