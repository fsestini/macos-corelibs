{-# LANGUAGE TypeSynonymInstances #-}

module MacSdk.Framework.CoreFoundation.String
  ( CFStringEncoding(..)
  , CFStringRef
  , CFString
  -- * Conversion
  , toCString
  , fromCString
  , fromCStringWithAllocator
  , toString
  , fromString
  ) where

import MacSdk.Framework.CoreFoundation.Allocator
import MacSdk.Framework.CoreFoundation.Object
import MacSdk.Framework.CoreFoundation.Array
import Foreign
import Foreign.C.String
import Foreign.C.Types (CBool(..), CInt(..))
import Control.Monad
import Control.Monad.IO.Class (MonadIO(..))

#include <Carbon/Carbon.h>

type ForeignCFStringEncoding = Word32

-- | String encodings to be used when converting from and to CFString references.
data CFStringEncoding
  = CFStringEncodingMacRoman
  -- ^ An encoding constant that identifies the Mac Roman encoding.
  | CFStringEncodingWindowsLatin1
  -- ^ An encoding constant that identifies the Windows Latin 1 encoding
  -- (ANSI codepage 1252).
  | CFStringEncodingISOLatin1
  -- ^ An encoding constant that identifies the ISO Latin 1 encoding
  -- (ISO 8859-1)
  | CFStringEncodingNextStepLatin
  -- ^ An encoding constant that identifies the NextStep/OpenStep encoding.
  | CFStringEncodingASCII
  -- ^ An encoding constant that identifies the ASCII encoding (decimal values 0
  -- through 127).
  | CFStringEncodingUnicode
  -- ^ An encoding constant that identifies the Unicode encoding.
  | CFStringEncodingUTF8
  -- ^ An encoding constant that identifies the UTF 8 encoding.
  | CFStringEncodingNonLossyASCII
  -- ^ An encoding constant that identifies non-lossy ASCII encoding.
  | CFStringEncodingUTF16
  -- ^ An encoding constant that identifies kTextEncodingUnicodeDefault +
  -- kUnicodeUTF16Format encoding (alias of kCFStringEncodingUnicode).
  | CFStringEncodingUTF16BE
  -- ^ An encoding constant that identifies kTextEncodingUnicodeDefault +
  -- kUnicodeUTF16BEFormat encoding. This constant specifies big-endian byte
  -- order.
  | CFStringEncodingUTF16LE
  -- ^ An encoding constant that identifies kTextEncodingUnicodeDefault +
  -- kUnicodeUTF16LEFormat encoding. This constant specifies little-endian byte
  -- order.
  | CFStringEncodingUTF32
  -- ^ An encoding constant that identifies kTextEncodingUnicodeDefault +
  -- kUnicodeUTF32Format encoding.
  | CFStringEncodingUTF32BE
  -- ^ An encoding constant that identifies kTextEncodingUnicodeDefault +
  -- kUnicodeUTF32BEFormat encoding. This constant specifies big-endian byte
  -- order.
  | CFStringEncodingUTF32LE
  -- ^ An encoding constant that identifies kTextEncodingUnicodeDefault +
  -- kUnicodeUTF32LEFormat encoding. This constant specifies little-endian byte
  -- order.

toForeignEncoding :: CFStringEncoding -> ForeignCFStringEncoding
toForeignEncoding = \case
  CFStringEncodingMacRoman -> (#const kCFStringEncodingMacRoman)
  CFStringEncodingWindowsLatin1 -> (#const kCFStringEncodingWindowsLatin1)
  CFStringEncodingISOLatin1 -> (#const kCFStringEncodingISOLatin1)
  CFStringEncodingNextStepLatin -> (#const kCFStringEncodingNextStepLatin)
  CFStringEncodingASCII -> (#const kCFStringEncodingASCII)
  CFStringEncodingUnicode -> (#const kCFStringEncodingUnicode)
  CFStringEncodingUTF8 -> (#const kCFStringEncodingUTF8)
  CFStringEncodingNonLossyASCII -> (#const kCFStringEncodingNonLossyASCII)
  CFStringEncodingUTF16 -> (#const kCFStringEncodingUTF16)
  CFStringEncodingUTF16BE -> (#const kCFStringEncodingUTF16BE)
  CFStringEncodingUTF16LE -> (#const kCFStringEncodingUTF16LE)
  CFStringEncodingUTF32 -> (#const kCFStringEncodingUTF32)
  CFStringEncodingUTF32BE -> (#const kCFStringEncodingUTF32BE)
  CFStringEncodingUTF32LE -> (#const kCFStringEncodingUTF32LE)

data CFString_
type CFStringRef = Ptr CFString_
instance CFClass CFString_
type CFString = Object CFString_

foreign import ccall unsafe "CFStringGetCString"
  cfStringGetCString
  :: CFStringRef -> CString -> CFIndex -> ForeignCFStringEncoding -> IO CBool

foreign import ccall "CFStringCreateWithCString" cfStringCreateWithCString
  :: CFAllocatorRef -> Ptr a -> ForeignCFStringEncoding -> IO CFStringRef

foreign import ccall "CFStringGetLength"
  cfStringGetLength :: CFStringRef -> IO CFIndex

foreign import ccall "CFStringGetMaximumSizeForEncoding"
  cfStringGetMaximumSizeForEncoding
  :: CFIndex -> ForeignCFStringEncoding -> IO CFIndex

-- | Creates a CFString reference from a C-style string, using a provided
-- allocator and string encoding.
fromCStringWithAllocator
  :: MonadIO m => Allocator -> CString -> CFStringEncoding -> m CFString
fromCStringWithAllocator allo cstr enc = liftIO $ withCFPtr allo $ \pallo ->
  cfStringCreateWithCString pallo cstr (toForeignEncoding enc) >>= manageCFObj

-- | Creates a CFString reference from an ASCII-encoded C-style string, using a
-- default allocator.
fromCString :: MonadIO m => CFStringEncoding -> CString -> m CFString
fromCString enc cstr =
  liftIO (nullAllocator >>= \allo -> fromCStringWithAllocator allo cstr enc)

-- | Creates a CFString reference from a regular string with ASCII encoding.
fromString :: MonadIO m => CFStringEncoding -> String -> m CFString
fromString enc str = liftIO (newCString str >>= fromCString enc)

-- | Converts a Core Foundation String to a C-style null-terminated string. It
-- returns 'Nothing' if the conversion is unsuccessfull.
toCString :: MonadIO m => CFStringEncoding -> CFString -> m (Maybe CString)
toCString enc cfs = liftIO . withCFPtr cfs $ \s -> do
  len <- cfStringGetLength s
  bytes <- cfStringGetMaximumSizeForEncoding len foreignEnc -- stringEncodingUTF8
  cstr <- mallocArray (fromIntegral bytes + 1)
  res <- cfStringGetCString s cstr (bytes + 1) foreignEnc -- stringEncodingUTF8
  if toBool res
    then pure (Just cstr)
    else free cstr >> pure Nothing
  where foreignEnc = toForeignEncoding enc

-- | Converts a CFString reference to a regular string.
toString :: MonadIO m => CFStringEncoding -> CFString -> m (Maybe String)
toString enc =
  liftIO . (maybe (pure Nothing) (fmap Just . peekCString) <=< toCString enc)
