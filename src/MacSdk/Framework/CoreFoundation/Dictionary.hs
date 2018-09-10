module MacSdk.Framework.CoreFoundation.Dictionary
  ( Dictionary
  , CFDictionaryRef
  , CFDictionaryKeyCallBacks
  , CFDictionaryValueCallBacks
  , createDictionary
  , getDictValue
  , getDictValue'
  , getDictValueSafe
  , stringDictionaryKeyCallBacks
  , cfTypeDictionaryValueCallBacks
  -- , strKey
  ) where

import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Managed
import Foreign.Ptr
import Foreign.C.Types (CInt(..), CBool(..))
import Foreign.Marshal.Utils (withMany)
import Foreign.Marshal.Array
import Foreign hiding (with)
import MacSdk.Framework.CoreFoundation.Object
import MacSdk.Framework.CoreFoundation.Array
import MacSdk.Framework.CoreFoundation.Allocator
import MacSdk.Framework.CoreFoundation.String

data CFDictionary
type CFDictionaryRef = Ptr CFDictionary
instance CFClass CFDictionary
type Dictionary = Object CFDictionary

foreign import ccall unsafe "cfCopyStringDictionaryKeyCallBacks"
  stringDictionaryKeyCallBacks :: IO (Ptr CFDictionaryKeyCallBacks)
foreign import ccall unsafe "cfTypeDictionaryValueCallBacks"
  cfTypeDictionaryValueCallBacks :: IO (Ptr CFDictionaryValueCallBacks)

data CFDictionaryKeyCallBacks
data CFDictionaryValueCallBacks

foreign import ccall unsafe "CFDictionaryCreate" cfDictionaryCreate
  :: CFAllocatorRef -> Ptr CFTypeRef -> Ptr CFTypeRef -> CFIndex
  -> Ptr CFDictionaryKeyCallBacks -> Ptr CFDictionaryValueCallBacks
  -> IO CFDictionaryRef

createDictionary
  :: CFClass obj => Allocator -> [CFString] -> [Object obj] -> IO Dictionary
createDictionary allo keys vals = flip with pure $ do
  allo' <- managed (withCFPtr allo)
  keys' <- fmap (fmap asCFType) $ managed (withMany withCFPtr keys)
  keys'' <- liftIO (newArray keys')
  vals' <- fmap (fmap asCFType) $ managed (withMany withCFPtr vals)
  vals'' <- liftIO (newArray vals')
  k1 <- liftIO stringDictionaryKeyCallBacks
  k2 <- liftIO cfTypeDictionaryValueCallBacks
  liftIO $
    cfDictionaryCreate allo' keys'' vals'' (fromIntegral (length keys)) k1 k2
      >>= manageCFObj

foreign import ccall unsafe "CFDictionaryGetValue"
  cfDictionaryGetValue :: CFDictionaryRef -> CFTypeRef -> IO (Ptr b)

-- | Returns the value associated with a given key.
getDictValue :: (CFClass key, CFClass val) => Dictionary -> Object key -> IO (Object val)
getDictValue dict key = flip with pure $ do
  d' <- managed (withCFPtr dict)
  k' <- managed (withCFPtr key)
  liftIO $ cfDictionaryGetValue d' (asCFType k') >>= retainManageCFObj

getDictValue' :: (MonadIO m, CFClass val) => Dictionary -> CFStringRef -> m (Object val)
getDictValue' dict key = liftIO $ withCFPtr dict $ \d ->
  cfDictionaryGetValue d (asCFType key) >>= retainManageCFObj

foreign import ccall unsafe "CFDictionaryGetValueIfPresent"
  cfDictionaryGetValueIfPresent
  :: CFDictionaryRef -> Ptr a -> Ptr (Ptr b) -> IO CBool

getDictValueSafe
  :: (MonadIO m, CFClass val)
  => Dictionary -> CFStringRef -> m (Maybe (Object val))
getDictValueSafe dict key = liftIO $ flip with pure $ do
  d <- managed (withCFPtr dict)
  p <- managed alloca
  liftIO $ do
    b <- toBool <$> cfDictionaryGetValueIfPresent d key p
    if b then peek p >>= fmap Just . retainManageCFObj else pure Nothing

-- strKey :: String -> IO S.String
-- strKey = ioTo

-- foreign import ccall azder :: CFDictionaryRef -> IO CFNumberRef

-- azder' :: Dictionary -> IO Number
-- azder' d = withCFPtr d azder >>= manageCFObj
