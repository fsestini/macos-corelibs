{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE KindSignatures #-}

module MacSdk.Framework.CoreFoundation.Number
  ( CFNumberRef
  , Number
  , NumberType(..)
  , numberCreateWithAllocator
  , numberCreate
  , numberGetValue
  ) where

import Control.Monad.Managed
import Control.Monad.IO.Class (MonadIO(..))
import Foreign hiding (with)
import Foreign.C.Types (CInt(..), CBool(..))
import MacSdk.Framework.CoreFoundation.Allocator
import MacSdk.Framework.CoreFoundation.Object

data CFNumber
type CFNumberRef = Ptr CFNumber
instance CFClass CFNumber
type Number = Object CFNumber

type CFNumberType = CInt

data NumberType :: * -> * where
  NumberSigned64Type :: NumberType Int64
  NumberLongType :: NumberType Int64
  NumberIntType :: NumberType Int32
  NumberSInt32Type :: NumberType Int32

storableTy :: NumberType ty -> (Storable ty => b) -> b
storableTy NumberSigned64Type f = f
storableTy NumberLongType f = f
storableTy NumberIntType f = f
storableTy NumberSInt32Type f = f

toCFNumberType :: NumberType ty -> CFNumberType
toCFNumberType NumberSigned64Type = 4
toCFNumberType NumberIntType = 9
toCFNumberType NumberLongType = 10
toCFNumberType NumberSInt32Type = 3

--------------------------------------------------------------------------------

foreign import ccall unsafe "CFNumberCreate"
  cfNumberCreate :: CFAllocatorRef -> CFNumberType -> Ptr a -> IO CFNumberRef

numberCreateWithAllocator :: Allocator -> NumberType ty -> ty -> IO Number
numberCreateWithAllocator allo nty num = flip with pure $ do
  allo' <- managed (withCFPtr allo)
  storableTy nty $ do
    p <- managed alloca
    liftIO $ do
      poke p num
      num' <- cfNumberCreate allo' (toCFNumberType nty) p
      manageCFObj num'

numberCreate :: NumberType ty -> ty -> IO Number
numberCreate nty num = nullAllocator >>= \a -> numberCreateWithAllocator a nty num

--------------------------------------------------------------------------------

foreign import ccall unsafe "CFNumberGetValue"
  cfNumberGetValue :: CFNumberRef -> CFNumberType -> Ptr a -> IO CBool

numberGetValue :: MonadIO m => Number -> NumberType ty -> m (Maybe ty)
numberGetValue num nty = liftIO $ flip with pure $ do
  nptr <- managed (withCFPtr num)
  storableTy nty $ do
    p <- managed alloca
    liftIO $ do
      b <- cfNumberGetValue nptr (toCFNumberType nty) p
      if toBool b then Just <$> peek p else pure Nothing
