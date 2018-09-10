
module MacSdk.Framework.Accessibility.Error
  ( AXError(..)
  , MonadError
  , AsAXError(..)
  , ForeignAXError
  , toAXResult
  , except
  , throwing
  ) where

#include <Carbon/Carbon.h>

import Foreign.C.Types (CInt(..))
import MacSdk.Prism
import Control.Monad.Error.Class (MonadError(..))
import Control.Monad (join)

type ForeignAXError = CInt

data AXError
  = AXErrorAPIDisabled
  -- ^ Assistive applications are not enabled in System Preferences.
  | AXErrorActionUnsupported
  -- ^ The referenced action is not supported. Alternatively, you can return the
  -- eventNotHandledErr error.
  | AXErrorAttributeUnsupported
  -- ^ The referenced attribute is not supported. Alternatively, you can return
  -- the eventNotHandledErr error.
  | AXErrorCannotComplete
  -- ^ A fundamental error has occurred, such as a failure to allocate memory
  -- during processing.
  | AXErrorFailure
  | AXErrorIllegalArgument
  -- ^ The value received in this event is an invalid value for this attribute.
  -- This also applies for invalid parameters in parameterized attributes.
  | AXErrorInvalidUIElement
  -- ^ The accessibility object received in this event is invalid.
  | AXErrorInvalidUIElementObserver
  -- ^ The observer for the accessibility object received in this event is
  -- invalid.
  | AXErrorNoValue
  | AXErrorNotEnoughPrecision
  | AXErrorNotImplemented
  | AXErrorNotificationAlreadyRegistered
  | AXErrorNotificationNotRegistered
  | AXErrorNotificationUnsupported
  | AXErrorParameterizedAttributeUnsupported
  -- ^ The parameterized attribute is not supported. Alternatively, you can
  -- return the eventNotHandledErr error.
  | AXErrorUnrecognized ForeignAXError
  deriving (Eq, Show)

toAXResult :: ForeignAXError -> Either AXError ()
toAXResult e
  | e == (#const kAXErrorAPIDisabled) = Left AXErrorAPIDisabled
  | e == (#const kAXErrorActionUnsupported) = Left AXErrorActionUnsupported
  | e == (#const kAXErrorAttributeUnsupported) = Left AXErrorAttributeUnsupported
  | e == (#const kAXErrorCannotComplete) = Left AXErrorCannotComplete
  | e == (#const kAXErrorFailure) = Left AXErrorFailure
  | e == (#const kAXErrorIllegalArgument) = Left AXErrorIllegalArgument
  | e == (#const kAXErrorInvalidUIElement) = Left AXErrorInvalidUIElement
  | e == (#const kAXErrorInvalidUIElementObserver) =
    Left AXErrorInvalidUIElementObserver
  | e == (#const kAXErrorNoValue) = Left AXErrorNoValue
  | e == (#const kAXErrorNotEnoughPrecision) = Left AXErrorNotEnoughPrecision
  | e == (#const kAXErrorNotImplemented) = Left AXErrorNotImplemented
  | e == (#const kAXErrorNotificationAlreadyRegistered) =
    Left AXErrorNotificationAlreadyRegistered
  | e == (#const kAXErrorNotificationNotRegistered) =
    Left AXErrorNotificationNotRegistered
  | e == (#const kAXErrorNotificationUnsupported) =
    Left AXErrorNotificationUnsupported
  | e == (#const kAXErrorParameterizedAttributeUnsupported) =
    Left AXErrorParameterizedAttributeUnsupported
  | e == (#const kAXErrorSuccess) = Right ()
  | otherwise = Left (AXErrorUnrecognized e)

class AsAXError e where
  _AXError :: Prism' e AXError

instance AsAXError AXError where
  _AXError = id

except :: (MonadError e m, AsAXError e) => m (Either AXError a) -> m a
except = join . fmap (either (throwing _AXError) pure)
