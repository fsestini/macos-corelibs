{-# LANGUAGE TypeSynonymInstances #-}

module MacSdk.Framework.CoreFoundation.Boolean
  ( Boolean
  , CFBooleanRef
  -- , MacSdk.Framework.CoreFoundation.Boolean.toBool
  -- , MacSdk.Framework.CoreFoundation.Boolean.fromBool
  , booleanFalse
  , booleanTrue
  , refToBool
  , boolToRef
  ) where

import MacSdk.Framework.CoreFoundation.Object
import qualified Foreign.C.Types as C (CChar(..))
import Foreign
import Data.Bool

data CFBoolean
type CFBooleanRef = Ptr CFBoolean
instance CFClass CFBoolean
type Boolean = Object CFBoolean

foreign import ccall unsafe cf_boolean_true :: CFBooleanRef
foreign import ccall unsafe cf_boolean_false :: CFBooleanRef
foreign import ccall unsafe "Carbon/Carbon.h CFBooleanGetValue"
  cf_boolean_get_value :: CFBooleanRef -> IO C.CChar

-- toBool :: CFBooleanRef -> IO Bool
-- toBool = fmap Foreign.toBool . cf_boolean_get_value

-- fromBool :: Bool -> CFBooleanRef
-- fromBool = bool cf_boolean_false cf_boolean_true

-- instance IOIso Bool CFBooleanRef where
boolToRef :: Bool -> IO CFBooleanRef
boolToRef = pure . bool cf_boolean_false cf_boolean_true

refToBool :: CFBooleanRef -> IO Bool
refToBool = fmap Foreign.toBool . cf_boolean_get_value

-- instance IOIso Bool Boolean where
  -- ioTo = bool booleanFalse booleanTrue
  -- ioFrom = flip withCFPtr refToBool

booleanFalse :: IO Boolean
booleanFalse = Object <$> newForeignPtr_ cf_boolean_false

booleanTrue :: IO Boolean
booleanTrue = Object <$> newForeignPtr_ cf_boolean_true
