module MacSdk.Framework.Accessibility.Types where

import Foreign
import MacSdk.Framework.CoreFoundation.Object

data AXUIElement
type AXUIElementRef = Ptr AXUIElement

-- | This type represents a reference to an accessible user interface element.
type UIElement = Object AXUIElement
instance CFClass AXUIElement
