-- | Core Graphics functions to control the mouse cursor.

module MacSdk.Framework.CoreGraphics.Mouse where

import MacSdk.Framework.CoreGraphics.Error
import MacSdk.Framework.CoreGraphics.Rect
import Foreign.C.Types
import Foreign
import Control.Monad.IO.Class(MonadIO(..))

-- | Moves the mouse cursor without generating events.
foreign import ccall unsafe "CGWarpMouseCursorPosition_"
  cgWarpMouseCursorPosition :: Ptr Point -> IO CInt

warpMouseCursorPosition :: (MonadIO m, MonadError e m, AsCGError e) => Point -> m ()
warpMouseCursorPosition p = except . liftIO $
  intToCGError <$> alloca (\ptr -> poke ptr p >> cgWarpMouseCursorPosition ptr)
