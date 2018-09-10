module MacSdk.Framework.CoreFoundation.Allocator
  ( Allocator
  , CFAllocatorRef
  , nullAllocator
  ) where

import Foreign
import MacSdk.Framework.CoreFoundation.Object

data CFAllocator
type CFAllocatorRef = Ptr CFAllocator
instance CFClass CFAllocator where
type Allocator = Object CFAllocator

nullAllocator :: IO Allocator
nullAllocator = Object <$> newForeignPtr_ nullPtr
