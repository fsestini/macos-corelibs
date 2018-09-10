{-# LANGUAGE ScopedTypeVariables #-}

module MacSdk.Framework.CoreFoundation.Object where

import Control.Monad.Managed
import Foreign hiding (with)
import Foreign.C.Types (CBool(..))
import Data.Proxy

data CFType_
type CFType = Object CFType_
type CFTypeRef = Ptr CFType_
type CFObject = Object CFType_
instance CFClass CFType_ where

type CFTypeID = Word64

class CFClass a where
  asCFType :: Ptr a -> Ptr CFType_
  asCFType = castPtr

class CFClass a => CFTypeable a where
  cfTypeID :: Proxy a -> IO CFTypeID

-- | A reference to a CoreFoundation object whose memory allocation is
-- automatically managed by Haskell.
newtype Object a = Object (ForeignPtr a)

withCFPtr :: Object a -> (Ptr a -> IO b) -> IO b
withCFPtr (Object p) = withForeignPtr p

-- | A reference to a CoreFoundation object whose memory allocation is manually
-- managed. It can be used to keep an object in memory beyond its garbage
-- collection. Note that failing to release a manual memory reference will cause
-- a memory leak.
newtype Ref = Ref { unRef :: CFTypeRef }

-- | Retains an object in memory, returning a corresponding reference.
--
-- An object can be retained multiple times, each time creating a new, distinct
-- memory reference. Every reference that is created with this function must be
-- explicitly released with 'refRelease' to avoid memory leaks.
objRetain :: CFClass a => Object a -> IO Ref
objRetain = flip withCFPtr $ \p ->
  let q = asCFType p in cf_retain q >> pure (Ref q)

-- | Releases a manual memory reference.
refRelease :: Ref -> IO ()
refRelease = cf_release . unRef

-- TODO: check that it is only used with references following the "copy rule"
manageCFObj :: Ptr a -> IO (Object a)
manageCFObj ptr =
  if ptr == nullPtr
    then error "manageCFObj: null pointer"
    else Object <$> newForeignPtr cfReleasePtr ptr

-- TODO: check that it is only used with references following the "create rule"
retainManageCFObj :: CFClass a => Ptr a -> IO (Object a)
retainManageCFObj ref = cfRetain ref >> manageCFObj ref

cfRetain :: CFClass a => Ptr a -> IO ()
cfRetain p = cf_retain (asCFType p)

foreign import ccall "Carbon/Carbon.h CFRetain" cf_retain :: CFTypeRef -> IO ()
foreign import ccall "&CFRelease" cfReleasePtr :: FunPtr (Ptr a -> IO ())
foreign import ccall "CFRelease" cf_release :: CFTypeRef -> IO ()

foreign import ccall "Carbon/Carbon.h CFEqual"
  cf_equal :: CFTypeRef -> CFTypeRef -> IO CBool

objEquals :: (CFClass a, MonadIO m) => Object a -> Object a -> m Bool
objEquals x y = liftIO . flip with pure $ do
  p <- asCFType <$> managed (withCFPtr x)
  q <- asCFType <$> managed (withCFPtr y)
  liftIO (fmap toBool (cf_equal p q))

cfEqual :: CFClass a => Ptr a -> Ptr a -> IO Bool
cfEqual x y = do
  res <- cf_equal (asCFType x) (asCFType y)
  pure (toBool res)

foreign import ccall "CFShow" cf_show :: CFTypeRef -> IO ()

objShow :: CFClass o => Object o -> IO ()
objShow = flip withCFPtr (cf_show . asCFType)

foreign import ccall "CFGetTypeID" cfGetTypeID :: CFTypeRef -> IO CFTypeID

getTypeID :: CFClass o => Object o -> IO CFTypeID
getTypeID = flip withCFPtr (cfGetTypeID . asCFType)

objCast :: forall o o'. (CFClass o, CFTypeable o')
        => Object o -> IO (Maybe (Object o'))
objCast o = do
  ty <- getTypeID o
  wanted <- cfTypeID (Proxy :: Proxy o')
  if ty == wanted
    then Just <$> withCFPtr o (retainManageCFObj . castPtr)
    else pure Nothing
