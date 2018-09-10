module MacSdk.Framework.CoreGraphics.Error
  ( intToCGError
  , CGError(..)
  , ForeignCGError
  , AsCGError(..)
  , MonadError
  , except
  ) where

#include <Carbon/Carbon.h>

import Foreign.C.Types (CInt(..))
import Control.Exception (Exception)
import Data.Typeable (Typeable)

import MacSdk.Prism
import Control.Monad.Error.Class (MonadError(..))
import Control.Monad (join)

data CGError
  = CGErrorCannotComplete
  | CGErrorFailure
  | CGErrorIllegalArgument
  | CGErrorInvalidConnection
  | CGErrorInvalidContext
  | CGErrorInvalidOperation
  | CGErrorNoneAvailable
  | CGErrorNotImplemented
  | CGErrorRangeCheck
  | CGErrorTypeCheck
  | CGErrorUnknown Int
  deriving (Show, Typeable)

instance Exception CGError

type ForeignCGError = CInt

intToCGError :: CInt -> Either CGError ()
intToCGError x
  | x == (#const kCGErrorCannotComplete) = Left CGErrorCannotComplete
  | x == (#const kCGErrorFailure) = Left CGErrorFailure
  | x == (#const kCGErrorIllegalArgument) = Left CGErrorIllegalArgument
  | x == (#const kCGErrorInvalidConnection) = Left CGErrorInvalidConnection
  | x == (#const kCGErrorInvalidContext) = Left CGErrorInvalidContext
  | x == (#const kCGErrorInvalidOperation) = Left CGErrorInvalidOperation
  | x == (#const kCGErrorNoneAvailable) = Left CGErrorNoneAvailable
  | x == (#const kCGErrorNotImplemented) = Left CGErrorNotImplemented
  | x == (#const kCGErrorRangeCheck) = Left CGErrorRangeCheck
  | x == (#const kCGErrorSuccess) = Right ()
  | x == (#const kCGErrorTypeCheck) = Left CGErrorTypeCheck
  | otherwise = Left (CGErrorUnknown (fromIntegral x))

class AsCGError e where
  _CGError :: Prism' e CGError

instance AsCGError CGError where
  _CGError = id

except :: (MonadError e m, AsCGError e) => m (Either CGError a) -> m a
except = join . fmap (either (throwing _CGError) pure)
