module MacSdk.Framework.CoreFoundation.Array
  ( CFArrayRef
  , CFIndex
  , Array(..)
  , createArray
  , createArray'
  , arrayValues
  , getCFArrayValues
  ) where

import MacSdk.Framework.CoreFoundation.Object
import MacSdk.Framework.CoreFoundation.Allocator
import Foreign.Ptr
import Foreign hiding (with)
import Foreign.C.Types (CInt(..))
import Control.Monad
import Control.Monad.Managed
import Data.Foldable

data CFArray
type CFArrayRef = Ptr CFArray
instance CFClass CFArray
newtype Array a = Array { getArray :: Object CFArray }
-- type Array a = ManagedObj CFArray
-- newtype Array a = Array (ForeignPtr CFArray) deriving (CFObject)
instance Functor Array where
  fmap _ (Array a) = Array a

type CFIndex = CInt

foreign import ccall unsafe "Carbon/Carbon.h CFArrayGetValueAtIndex"
  cf_array_get_value_at_index :: CFArrayRef -> CFIndex -> IO (Ptr ())

foreign import ccall unsafe "Carbon/Carbon.h CFArrayGetCount"
  cf_array_get_count :: CFArrayRef -> IO CFIndex

getCFArrayValues :: CFArrayRef -> IO [CFTypeRef]
getCFArrayValues arr = do
  n <- cf_array_get_count arr
  ptrs <- forM [0.. (n-1)] (cf_array_get_value_at_index arr)
  pure (fmap castPtr ptrs)

arrayValues :: MonadIO m => Array (Object a) -> m [Object a]
arrayValues arr = liftIO . withCFPtr (getArray arr) $ \cfarr ->
  getCFArrayValues cfarr >>= mapM manageCFObj . fmap castPtr

--------------------------------------------------------------------------------
-- Array creation

data ArrayCallbacks

foreign import ccall cfCallbs :: Ptr ArrayCallbacks

foreign import ccall "CFArrayCreate"
  cfArrayCreate :: CFAllocatorRef -> Ptr CFTypeRef -> CFIndex
                -> Ptr ArrayCallbacks -> IO CFArrayRef

-- | Create an array from a denumerable collection of Core Foundation objects,
-- using the specified allocator.
createArray :: (Foldable f, CFClass a) => Allocator -> f (Object a) -> IO (Array (Object a))
createArray allo objs = fmap Array . flip with pure $ do
  allo' <- managed (withCFPtr allo)
  objs' <- managed (withMany withCFPtr (toList objs))
  liftIO $ do
    arr <- newArray (fmap asCFType objs')
    cfArrayCreate allo' arr (fromIntegral (length objs')) cfCallbs
      >>= manageCFObj

-- | Create an array from a denumerable collection of Core Foundation objects,
-- using the default allocator.
createArray' :: (Foldable f, CFClass a) => f (Object a) -> IO (Array (Object a))
createArray' objs = nullAllocator >>= flip createArray objs
