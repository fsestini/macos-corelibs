{-| Run loops for system-wide events. -}

module MacSdk.Framework.CoreFoundation.RunLoop
  ( CFRunLoopSourceRef
  , RunLoop
  , RunLoopSource
  , RunLoopMode(..)
  , runLoopContainsSource
  , runLoopGetCurrent
  , runLoopAddSource
  , mainRunLoop
  , runLoopRun
  , runLoopStop
  , runLoopSourceInvalidate
  , machPortRunLoopSource
  , addSourceToMainLoop
  -- * Mach ports
  , CFMachPortRef
  , MachPort
  , eventTapIsEnabled
  , enableEventTap
  , cgEventTapIsEnabled
  -- , releaseMachPort
  ) where

import Control.Monad.Managed
import MacSdk.Framework.CoreFoundation.Object
import MacSdk.Framework.CoreFoundation.String
import MacSdk.Framework.CoreFoundation.Allocator
import MacSdk.Framework.CoreFoundation.Array
import Foreign hiding (with)
import Foreign.C.Types (CBool(..), CInt(..))
import Control.Monad
import System.IO.Unsafe (unsafePerformIO)

data CFMachPort
type CFMachPortRef = Ptr CFMachPort
instance CFClass CFMachPort where
type MachPort = Object CFMachPort

foreign import ccall unsafe "CGEventTapIsEnabled"
  cgEventTapIsEnabled :: CFMachPortRef -> IO CBool

eventTapIsEnabled :: MachPort -> IO Bool
eventTapIsEnabled = fmap toBool . flip withCFPtr cgEventTapIsEnabled

foreign import ccall unsafe "CGEventTapEnable"
  cgEventTapEnable :: CFMachPortRef -> CBool -> IO ()

enableEventTap :: MachPort -> Bool -> IO ()
enableEventTap mp b = withCFPtr mp $ flip cgEventTapEnable (fromBool b)

-- releaseMachPort :: MachPort -> IO ()
-- releaseMachPort = flip withCFPtr cfRelease

data CFRunLoop
type CFRunLoopRef = Ptr CFRunLoop
instance CFClass CFRunLoop

-- | Type of run loops.
--
-- A run loop is an event loop in a (system) thread that looks for events, such
-- as mouse clicks, key presses, and timer firings. The run loop dispatches
-- these events to interested parties and then goes to sleep waiting for more
-- events. [Advanced Mac OS X Programming, 1st ed.]
type RunLoop = Object CFRunLoop

foreign import ccall unsafe kCFRunLoopCommonModes_ :: IO CFRunLoopMode
foreign import ccall unsafe kCFRunLoopDefaultMode_ :: IO CFRunLoopMode

type CFRunLoopMode = CFStringRef
data RunLoopMode
  = CommonModes
  -- ^ Objects added to a run loop using this value as the mode are monitored
  -- by all run loop modes that have been declared as a member of the set of
  -- “common” modes with 'runLoopAddCommonMode'.
  | DefaultMode
  -- ^ Run loop mode that should be used when a thread is in its default, or
  -- idle, state, waiting for an event. This mode is used when the run loop is
  -- started with 'runLoopRun'.

toCFRunLoopMode :: RunLoopMode -> CFRunLoopMode
toCFRunLoopMode = unsafePerformIO . \case
  CommonModes -> kCFRunLoopCommonModes_
  DefaultMode -> kCFRunLoopDefaultMode_

data CFRunLoopSource
type CFRunLoopSourceRef = Ptr CFRunLoopSource
instance CFClass CFRunLoopSource
type RunLoopSource = Object CFRunLoopSource

foreign import ccall unsafe "Carbon/Carbon.h CFRunLoopContainsSource"
  cfRunLoopContainsSource
  :: CFRunLoopRef -> CFRunLoopSourceRef -> CFStringRef -> IO CBool

runLoopContainsSource :: RunLoop -> RunLoopSource -> RunLoopMode -> IO Bool
runLoopContainsSource rl rls mode = flip with pure $ do
  rl' <- managed $ withCFPtr rl
  rls' <- managed $ withCFPtr rls
--   str' <- managed $ withCFPtr str
  liftIO . fmap toBool $ cfRunLoopContainsSource rl' rls' (toCFRunLoopMode mode) -- str'

foreign import ccall unsafe "Carbon/Carbon.h CFRunLoopAddSource"
  cfRunLoopAddSource
  :: CFRunLoopRef -> CFRunLoopSourceRef -> CFStringRef -> IO ()

runLoopAddSource :: RunLoop -> RunLoopSource -> RunLoopMode -> IO ()
runLoopAddSource rl rls mode = runManaged $ do
  rl' <- managed $ withCFPtr rl
  rls' <- managed $ withCFPtr rls
--  str' <- managed $ withCFPtr str
  liftIO $ cfRunLoopAddSource rl' rls' (toCFRunLoopMode mode) -- str'

foreign import ccall unsafe "Carbon/Carbon.h CFRunLoopGetMain"
  cfRunLoopGetMain :: IO CFRunLoopRef

foreign import ccall unsafe "Carbon/Carbon.h CFRunLoopGetCurrent"
  cfRunLoopGetCurrent :: IO CFRunLoopRef

-- | Returns the RunLoop object for the current thread.
runLoopGetCurrent :: IO RunLoop
runLoopGetCurrent = cfRunLoopGetCurrent >>= retainManageCFObj

-- | Runs the current thread’s CFRunLoop object in its default mode
-- indefinitely.
foreign import ccall "Carbon/Carbon.h CFRunLoopRun"
  runLoopRun :: IO ()

-- | Forces a CFRunLoop object to stop running.
foreign import ccall "Carbon/Carbon.h CFRunLoopStop"
  cfRunLoopStop :: CFRunLoopRef -> IO ()

runLoopStop :: RunLoop -> IO ()
runLoopStop l = withCFPtr l cfRunLoopStop

-- | Returns the main RunLoop object.
mainRunLoop :: IO RunLoop
mainRunLoop = cfRunLoopGetMain >>= retainManageCFObj

foreign import ccall unsafe "Carbon/Carbon.h CFRunLoopSourceInvalidate"
  cfRunLoopSourceInvalidate :: CFRunLoopSourceRef -> IO ()

runLoopSourceInvalidate :: RunLoopSource -> IO ()
runLoopSourceInvalidate = flip withCFPtr cfRunLoopSourceInvalidate

foreign import ccall "CFMachPortCreateRunLoopSource"
  cfMachPortCreateRunLoopSource
  :: CFAllocatorRef -> CFMachPortRef -> CFIndex -> IO CFRunLoopSourceRef

machPortRunLoopSource :: Allocator -> MachPort -> Int -> IO RunLoopSource
machPortRunLoopSource al mp i = flip with pure $ do
  al' <- managed $ withCFPtr al
  mp' <- managed $ withCFPtr mp
  liftIO $ cfMachPortCreateRunLoopSource al' mp' (fromIntegral i) >>= manageCFObj

foreign import ccall unsafe "mykCFRunLoopCommonModes"
  cfRunLoopCommonModes :: IO CFRunLoopMode

-- | Creates a loop source from the given mach port, and adds it to the main run
-- loop with default parameters.
addSourceToMainLoop :: MachPort -> IO ()
addSourceToMainLoop port = nullAllocator >>= \alloc -> 
  join $ runLoopAddSource <$> mainRunLoop
                          <*> machPortRunLoopSource alloc port 0
                          <*> pure CommonModes
