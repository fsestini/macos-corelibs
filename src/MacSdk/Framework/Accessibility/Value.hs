{-# LANGUAGE TypeFamilies #-}

module MacSdk.Framework.Accessibility.Value where

import MacSdk.Framework.CoreFoundation.Object
import Foreign.Ptr

data AXValue
type AXValueRef = Ptr AXValue
type Value = Object AXValue

instance CFClass AXValue
