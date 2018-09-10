module MacSdk.Framework.CoreFoundation.UUID
  ( CFUUIDRef
  , UUID
  ) where

import Foreign
import MacSdk.Framework.CoreFoundation.Object

data CFUUID
type CFUUIDRef = Ptr CFUUID
instance CFClass CFUUID where
type UUID = Object CFUUID
