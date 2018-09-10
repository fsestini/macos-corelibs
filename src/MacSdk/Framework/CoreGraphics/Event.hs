{-|

Quarts Event Services. This module provides features for managing event
taps—filters for observing and altering the stream of low-level user input
events in macOS.

See: https://developer.apple.com/documentation/coregraphics/quartz_event_services?language=objc

-}

module MacSdk.Framework.CoreGraphics.Event
  ( Event
  , EventType(..)
  , EventField(..)
  , EventSource
  , EventTapLocation(..)
  , EventTapCallback
  , MouseButtonPress(..)
  , KeyEventPosition(..)
  , CGKeyCode
  , module MacSdk.Framework.CoreGraphics.Event.Flag
  -- * Events
  , eventCreate
  , createMouseEvent
  , createKeyboardEvent
  , eventFlags
  , getEventField
  , eventGetType
  , eventSetType
  , eventSetFlags
  -- * Event taps
  , EventTap
  , eventTapEnable
  , eventTapDisable
  , eventPost
  , eventTapCreate
  , eventTapMachPort
  , eventTapRelease
  , QuartzEvents(..)
  , kCGSessionEventTap
  , kCGHeadInsertEventTap
  , kCGEventTapOptionDefault
  ) where

import Foreign.C.Types (CInt(..), CBool(..))
import Foreign.Ptr
import Foreign
import System.IO.Unsafe
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad (guard)
import MacSdk.Framework.CoreFoundation
import MacSdk.Framework.CoreGraphics.Rect
import MacSdk.Framework.CoreGraphics.Event.Type
import MacSdk.Framework.CoreGraphics.Event.Flag
import MacSdk.Framework.CoreGraphics.Event.Field
import MacSdk.Framework.CoreGraphics.Event.Mask
import MacSdk.Framework.CoreGraphics.Event.Location

data CGEvent
type CGEventRef = Ptr CGEvent
-- | Type of low-level Quartz events. A typical event in macOS originates when
-- the user manipulates an input device such as a mouse or a keyboard. You can
-- use event taps to gain access to Quartz events.
type Event = Object CGEvent
instance CFClass CGEvent
  
data CGEventSource
type CGEventSourceRef = Ptr CGEventSource
-- | Type of objects representing the source of a Quartz event.
type EventSource = Object CGEventSource
instance CFClass CGEventSource

--------------------------------------------------------------------------------
-- Working with Events

foreign import ccall unsafe "CGEventCreate"
  cg_event_create :: CGEventSourceRef -> IO CGEventRef

-- | Returns a new Quartz event to be filled in. If no event source is
-- specified, a default source is used. Returns 'Nothing' if the event could not
-- be created.
eventCreate :: Maybe EventSource -> IO (Maybe Event)
eventCreate = maybe (aux nullPtr) (flip withCFPtr aux)
  where
    aux p = do
      ref <- cg_event_create p
      if ref == nullPtr
        then pure Nothing
        else Just <$> manageCFObj ref

type CGMouseButton = Word32
data MouseButtonPress = LeftButtonPress | RightButtonPress | CenterButtonPress
toPressCode :: MouseButtonPress -> CGMouseButton
toPressCode = \case
  LeftButtonPress -> 0
  RightButtonPress -> 1
  CenterButtonPress -> 2

foreign import ccall unsafe "CGEventCreateMouseEvent_"
  cg_event_create_mouse_event
  :: CGEventSourceRef -> CGEventType -> Ptr Point -> CGMouseButton -> IO CGEventRef

-- | Returns a new Quartz mouse event. If no event source is specified, a
-- default source is used. Returns 'Nothing' if the event could not be created.
createMouseEvent
  :: Maybe EventSource -> EventType -> Point -> MouseButtonPress -> IO (Maybe Event)
createMouseEvent mes ety p mbp = maybe (aux nullPtr) (flip withCFPtr aux) mes
  where aux es = alloca $ \ptr -> do
          poke ptr p
          ref <- cg_event_create_mouse_event es
                   (toEventTypeCode ety) ptr (toPressCode mbp)
          if ref == nullPtr then pure Nothing else Just <$> manageCFObj ref

--------------------------------------------------------------------------------

type CGKeyCode = Word32

foreign import ccall unsafe "CGEventCreateKeyboardEvent"
  cg_event_create_keyboard_event
  :: CGEventSourceRef -> CGKeyCode -> CBool -> IO CGEventRef

data KeyEventPosition = KeyDown | KeyUp

createKeyboardEvent
  :: MonadIO m
  => Maybe EventSource -> CGKeyCode -> KeyEventPosition -> m (Maybe Event)
createKeyboardEvent mes k pos =
  liftIO $ maybe (aux nullPtr) (flip withCFPtr aux) mes
  where aux es = do
          res <- cg_event_create_keyboard_event es k $
            case pos of { KeyDown -> 1 ; KeyUp -> 0 }
          if res == nullPtr then pure Nothing else Just <$> manageCFObj res

--------------------------------------------------------------------------------

foreign import ccall unsafe "CGEventSetFlags"
  cg_event_set_flags :: CGEventRef -> CGEventFlags -> IO ()

eventSetFlags :: MonadIO m => Event -> CGEventFlags -> m ()
eventSetFlags e flags =
  liftIO $ withCFPtr e (flip cg_event_set_flags flags)

--------------------------------------------------------------------------------

foreign import ccall unsafe "CGEventPost"
  cg_event_post :: CGEventTapLocation -> CGEventRef -> IO ()

-- | Posts a Quartz event into the event stream at a specified location.
eventPost :: MonadIO m => EventTapLocation -> Event -> m ()
eventPost loc ev =
  liftIO $ withCFPtr ev (cg_event_post (toCGEventTapLocation loc))

--------------------------------------------------------------------------------

foreign import ccall unsafe "CGEventSetType"
  cg_event_set_type :: CGEventRef -> CGEventType -> IO ()

-- | Sets the event type of a Quartz event.
eventSetType :: MonadIO m => Event -> EventType -> m ()
eventSetType ev ety =
  liftIO $ withCFPtr ev (flip cg_event_set_type (toEventTypeCode ety))

--------------------------------------------------------------------------------

type CGEventType = Word32

foreign import ccall unsafe "CGEventGetType"
  cg_event_get_type :: CGEventRef -> IO CGEventType

-- | Returns the event type of a Quartz event (left mouse down, for example).
eventGetType :: Event -> IO EventType
eventGetType = flip withCFPtr (fmap fromEventTypeCode . cg_event_get_type)

--------------------------------------------------------------------------------

foreign import ccall unsafe "CGSIsSecureEventInputSet"
  check_secure_input_set :: IO CBool

checkSecureInputSet :: IO Bool
checkSecureInputSet = fmap Foreign.toBool check_secure_input_set

--------------------------------------------------------------------------------

foreign import ccall unsafe "CGEventGetFlags"
  cgEventGetFlags :: CGEventRef -> IO CGEventFlags

-- | Returns the event flags of a Quartz event.
eventFlags :: Event -> IO CGEventFlags
eventFlags = flip withCFPtr cgEventGetFlags

--------------------------------------------------------------------------------

type CGEventField = Word32

foreign import ccall unsafe "CGEventGetIntegerValueField"
  cgEventGetIntegerValueField :: CGEventRef -> CGEventField -> IO Word64

foreign import ccall unsafe "CGEventGetDoubleValueField"
  cgEventGetDoubleValueField :: CGEventRef -> CGEventField -> IO Double

getEventField :: Event -> EventField ty -> IO ty
getEventField e efs = withCFPtr e $ \p ->
  let code = toEventFieldCode efs
  in decideFieldType (cgEventGetIntegerValueField p code)
                     (cgEventGetDoubleValueField p code) efs

--------------------------------------------------------------------------------

foreign import ccall unsafe "CGEventTapEnable"
  cg_event_tap_enable :: CFMachPortRef -> CBool -> IO ()

-- | Enables an event tap.
--
-- Event taps are normally enabled when created. If an event tap becomes
-- unresponsive, or if a user requests that event taps be disabled, then a
-- EventTapDisabled event is passed to the event tap callback function. Event
-- taps may be re-enabled by calling this function.
eventTapEnable :: MonadIO m => EventTap -> m ()
eventTapEnable = liftIO . flip withCFPtr (`cg_event_tap_enable` 1) . _etMachPort

-- | Disables an event tap.
eventTapDisable :: MonadIO m => EventTap -> m ()
eventTapDisable = liftIO . flip withCFPtr (`cg_event_tap_enable` 0) . _etMachPort

--------------------------------------------------------------------------------

type CGEventTapPlacement = CInt
type CGEventTapOptions = CInt

foreign import ccall unsafe mykCGSessionEventTap :: IO CGEventTapLocation
kCGSessionEventTap :: CGEventTapLocation
kCGSessionEventTap = unsafePerformIO mykCGSessionEventTap
{-# NOINLINE kCGSessionEventTap #-}

foreign import ccall unsafe mykCGHeadInsertEventTap :: IO CGEventTapPlacement
kCGHeadInsertEventTap :: CGEventTapPlacement
kCGHeadInsertEventTap = unsafePerformIO mykCGHeadInsertEventTap
{-# NOINLINE kCGHeadInsertEventTap #-}

foreign import ccall unsafe mykCGEventTapOptionDefault :: IO CGEventTapOptions
kCGEventTapOptionDefault :: CGEventTapOptions
kCGEventTapOptionDefault = unsafePerformIO mykCGEventTapOptionDefault
{-# NOINLINE kCGEventTapOptionDefault #-}

data CGEventTapProxy_
type CGEventTapProxy = Ptr CGEventTapProxy_

-- | Type that represents state within the client application that’s associated
-- with an event tap.
newtype EventTapProxy = EventTapProxy { getEventTapProxy :: CGEventTapProxy }

type CGEventTapCallBack =
  CGEventTapProxy -> CGEventType -> CGEventRef -> Ptr () -> IO CGEventRef

-- | Type of client-supplied callback function that’s invoked whenever an
-- associated event tap receives a Quartz event.
-- 
-- An event tap proxy object is passed to the callback function when it receives
-- a new Quartz event. The function needs the proxy to post Quartz events using
-- the function 'eventTapPostEvent'.
--
-- If the event tap is an active filter, your callback function should return
-- one of the following:
--
-- * The (possibly modified) event that is passed in, or a newly-constructed
-- event, wrapped in 'Just'. This event is passed back to the event system.
-- 
-- * 'Nothing' if the event passed in is to be deleted.
--
-- If the event tap is an passive listener, the callback function may return the
-- event that is passed in wrapped in 'Just', or 'Nothing'. In either case, the
-- event stream is not affected.
type EventTapCallback a =
  EventTapProxy -> EventType -> Event -> a -> IO (Maybe Event)

foreign import ccall "wrapper"
  wrapCGEventTapCallBack :: CGEventTapCallBack -> IO (FunPtr CGEventTapCallBack)

foreign import ccall "CGEventTapCreate" cgEventTapCreate
  :: CGEventTapLocation -> CGEventTapPlacement -> CGEventTapOptions
  -> CGEventMask -> FunPtr CGEventTapCallBack -> Ptr () -> IO CFMachPortRef

data EventTap = EventTap
  { _etMachPort :: MachPort
  , _etMachPortRef :: Ref
  , _etCallb :: FunPtr CGEventTapCallBack
  }

eventTapMachPort :: EventTap -> MachPort
eventTapMachPort = _etMachPort

eventTapRelease :: MonadIO m => EventTap -> m ()
eventTapRelease (EventTap _ ref cb) =
  liftIO (refRelease ref >> freeHaskellFunPtr cb)

-- | Returns a newly-created 'EventTap', or 'Nothing' if the event tap could not
-- be created. When you are finished using the event tap, you should release it
-- using the function 'eventTapRelease'.
--
-- Event taps receive key up and key down events if one of the following
-- conditions is true:
--
-- * The current process is running as the root user.
--
-- * Access for assistive devices is enabled. In OS X v10.4, you can enable this
-- feature using System Preferences, Universal Access panel, Keyboard view. You
-- should check whether AX privileges have been granted with 'checkAXPrivileges'.
-- 
-- After creating an event tap, you can add it to a run loop as follows:
--
-- * Retrieve the event tap's 'MachPort' with 'eventTapMachPort' and pass it to
-- the 'machPortRunLoopSource' function to create a run loop event source.
--
-- * Call the 'runLoopAddSource' function to add the source to the appropriate
-- run loop.
-- 
-- Alternatively, you can 'addSourceToMainLoop' to start the event tap with
-- default parameters.
eventTapCreate
  :: CGEventTapLocation -> CGEventTapPlacement -> CGEventTapOptions
  -> QuartzEvents -> EventTapCallback a -> a -> IO (Maybe EventTap)
eventTapCreate l p o evs callb userData = do
  ptr <- castStablePtrToPtr <$> newStablePtr userData
  c' <- wrapCGEventTapCallBack $ \proxy ety e ptr' -> do
    ev <- retainManageCFObj e
    x <- deRefStablePtr (castPtrToStablePtr ptr')
    me <- callb (EventTapProxy proxy) (fromEventTypeCode ety) ev x
    maybe (pure nullPtr) (flip withCFPtr pure) me
  machPtr <- cgEventTapCreate l p o (getEventMask (toMask evs)) c' ptr
  mach <- manageCFObj machPtr
  ref <- objRetain mach
  -- Event taps are normally enabled when created. If not, something went wrong
  -- and we fail.
  en <- toBool <$> cgEventTapIsEnabled machPtr
  pure $ guard (machPtr /= nullPtr && en) >> Just (EventTap mach ref c')
