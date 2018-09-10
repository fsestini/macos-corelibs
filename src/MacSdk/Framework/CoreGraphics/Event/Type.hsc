{-# LANGUAGE LambdaCase #-}

{-# OPTIONS_GHC -Wno-missing-signatures #-}

-- | Constants and ADTs that specify the different types of input events.

module MacSdk.Framework.CoreGraphics.Event.Type where

#include <Carbon/Carbon.h>

import Data.Word

-- | The different types of Quartz input events.
data EventType
  = EventNull
  -- ^ Specifies a null event.
  | EventLeftMouseDown
  -- ^ Specifies a mouse down event with the left button.
  | EventLeftMouseUp
  -- ^ Specifies a mouse up event with the left button.
  | EventRightMouseDown
  -- ^ Specifies a mouse down event with the right button.
  | EventRightMouseUp
  -- ^ Specifies a mouse up event with the right button.
  | EventMouseMoved
  -- ^ Specifies a mouse moved event.
  | EventLeftMouseDragged
  -- ^ Specifies a mouse drag event with the left button down.
  | EventRightMouseDragged
  -- ^ Specifies a mouse drag event with the right button down.
  | EventKeyDown
  -- ^ Specifies a key down event.
  | EventKeyUp
  -- ^ Specifies a key up event.
  | EventFlagsChanged
  -- ^ Specifies a key changed event for a modifier or status key.
  | EventScrollWheel
  -- ^ Specifies a scroll wheel moved event.
  | EventTabletPointer
  -- ^ Specifies a tablet pointer event.
  | EventTabletProximity
  -- ^ Specifies a tablet proximity event.
  | EventOtherMouseDown
  -- ^ Specifies a mouse down event with one of buttons 2-31.
  | EventOtherMouseUp
  -- ^ Specifies a mouse up event with one of buttons 2-31.
  | EventOtherMouseDragged
  -- ^ Specifies a mouse drag event with one of buttons 2-31 down.
  | EventTapDisabledByTimeout
  -- ^ Specifies an event indicating the event tap is disabled because of timeout.
  | EventTapDisabledByUserInput
  -- ^ Specifies an event indicating the event tap is disabled because of user input.

eventNull = #const kCGEventNull
eventLeftMouseDown = #const kCGEventLeftMouseDown
eventLeftMouseUp = #const kCGEventLeftMouseUp
eventRightMouseDown = #const kCGEventRightMouseDown
eventRightMouseUp = #const kCGEventRightMouseUp
eventMouseMoved = #const kCGEventMouseMoved
eventLeftMouseDragged = #const kCGEventLeftMouseDragged
eventRightMouseDragged = #const kCGEventRightMouseDragged
eventKeyDown = #const kCGEventKeyDown
eventKeyUp = #const kCGEventKeyUp
eventFlagsChanged = #const kCGEventFlagsChanged
eventScrollWheel = #const kCGEventScrollWheel
eventTabletPointer = #const kCGEventTabletPointer
eventTabletProximity = #const kCGEventTabletProximity
eventOtherMouseDown = #const kCGEventOtherMouseDown
eventOtherMouseUp = #const kCGEventOtherMouseUp
eventOtherMouseDragged = #const kCGEventOtherMouseDragged
eventTapDisabledByTimeout = #const kCGEventTapDisabledByTimeout
eventTapDisabledByUserInput = #const kCGEventTapDisabledByUserInput

toEventTypeCode :: EventType -> Word32
toEventTypeCode = \case
  EventNull -> eventNull
  EventLeftMouseDown -> eventLeftMouseDown
  EventLeftMouseUp -> eventLeftMouseUp
  EventRightMouseDown -> eventRightMouseDown
  EventRightMouseUp -> eventRightMouseUp
  EventMouseMoved -> eventMouseMoved
  EventLeftMouseDragged -> eventLeftMouseDragged
  EventRightMouseDragged -> eventRightMouseDragged
  EventKeyDown -> eventKeyDown
  EventKeyUp -> eventKeyUp
  EventFlagsChanged -> eventFlagsChanged
  EventScrollWheel -> eventScrollWheel
  EventTabletPointer -> eventTabletPointer
  EventTabletProximity -> eventTabletProximity
  EventOtherMouseDown -> eventOtherMouseDown
  EventOtherMouseUp -> eventOtherMouseUp
  EventOtherMouseDragged -> eventOtherMouseDragged
  EventTapDisabledByTimeout -> eventTapDisabledByTimeout
  EventTapDisabledByUserInput -> eventTapDisabledByUserInput

fromEventTypeCode :: Word32 -> EventType
fromEventTypeCode code
  | code == eventNull = EventNull
  | code == eventLeftMouseDown = EventLeftMouseDown
  | code == eventLeftMouseUp = EventLeftMouseUp
  | code == eventRightMouseDown = EventRightMouseDown
  | code == eventRightMouseUp = EventRightMouseUp
  | code == eventMouseMoved = EventMouseMoved
  | code == eventLeftMouseDragged = EventLeftMouseDragged
  | code == eventRightMouseDragged = EventRightMouseDragged
  | code == eventKeyDown = EventKeyDown
  | code == eventKeyUp = EventKeyUp
  | code == eventFlagsChanged = EventFlagsChanged
  | code == eventScrollWheel = EventScrollWheel
  | code == eventTabletPointer = EventTabletPointer
  | code == eventTabletProximity = EventTabletProximity
  | code == eventOtherMouseDown = EventOtherMouseDown
  | code == eventOtherMouseUp = EventOtherMouseUp
  | code == eventOtherMouseDragged = EventOtherMouseDragged
  | code == eventTapDisabledByTimeout = EventTapDisabledByTimeout
  | code == eventTapDisabledByUserInput = EventTapDisabledByUserInput
  | otherwise = error "fromEventTypeCode: unrecognized event type code"
