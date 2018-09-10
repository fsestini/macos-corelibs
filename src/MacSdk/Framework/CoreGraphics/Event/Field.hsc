{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

-- | Constants and ADTs used as keys to access specialized fields in low-level
-- Quartz events.

module MacSdk.Framework.CoreGraphics.Event.Field where

#include <Carbon/Carbon.h>

import Data.Word

data EventField :: * -> * where
  -- | Key to access an integer field that contains the mouse button event number.
  -- Matching mouse-down and mouse-up events will have the same event number.
  MouseEventNumber :: EventField Word64

  -- | Key to access an integer field that contains the mouse button click state.
  -- A click state of 1 represents a single click. A click state of 2 represents a
  -- double-click. A click state of 3 represents a triple-click.
  MouseEventClickState :: EventField Word64

  -- | Key to access a double field that contains the mouse button pressure. The
  -- pressure value may range from 0 to 1, with 0 representing the mouse being up.
  -- This value is commonly set by tablet pens mimicking a mouse.
  MouseEventPressure :: EventField Double

  -- | Key to access an integer field that contains the mouse button number. For
  -- information about the possible values, see CGMouseButton.
  MouseEventButtonNumber :: EventField Word64

  -- | Key to access an integer field that contains the horizontal mouse delta
  -- since the last mouse movement event.
  MouseEventDeltaX :: EventField Word64

  -- | Key to access an integer field that contains the vertical mouse delta since
  -- the last mouse movement event.
  MouseEventDeltaY :: EventField Word64

  -- | Key to access an integer field. The value is non-zero if the event should
  -- be ignored by the Inkwell subsystem.
  MouseEventInstantMouser :: EventField Word64

  -- | Key to access an integer field that encodes the mouse event subtype as a
  -- kCFNumberIntType.
  MouseEventSubtype :: EventField Word64

  -- | Key to access an integer field, non-zero when this is an autorepeat of a
  -- key-down, and zero otherwise.
  KeyboardEventAutorepeat :: EventField Word64

  -- | Key to access an integer field that contains the virtual keycode of the
  -- key-down or key-up event.
  KeyboardEventKeycode :: EventField Word64

  -- | Key to access an integer field that contains the keyboard type identifier.
  KeyboardEventKeyboardType :: EventField Word64

  -- | Key to access an integer field that contains scrolling data. This field
  -- typically contains the change in vertical position since the last scrolling
  -- event from a Mighty Mouse scroller or a single-wheel mouse scroller.
  ScrollWheelEventDeltaAxis1 :: EventField Double

  -- | Key to access an integer field that contains scrolling data. This field
  -- typically contains the change in horizontal position since the last scrolling
  -- event from a Mighty Mouse scroller.
  ScrollWheelEventDeltaAxis2 :: EventField Double

  -- | Key to access a field that contains scrolling data. The scrolling data
  -- represents a line-based or pixel-based change in vertical position since the
  -- last scrolling event from a Mighty Mouse scroller or a single-wheel mouse
  -- scroller. The scrolling data uses a fixed-point 16.16 signed integer format.
  -- For example, if the field contains a value of 1.0, the integer 0x00010000 is
  -- returned by CGEventGetIntegerValueField. If this key is passed to
  -- CGEventGetDoubleValueField, the fixed-point value is converted to a double
  -- value.
  ScrollWheelEventFixedPtDeltaAxis1 :: EventField Word64

  -- | Key to access a field that contains scrolling data. The scrolling data
  -- represents a line-based or pixel-based change in horizontal position since
  -- the last scrolling event from a Mighty Mouse scroller. The scrolling data
  -- uses a fixed-point 16.16 signed integer format. For example, if the field
  -- contains a value of 1.0, the integer 0x00010000 is returned by
  -- CGEventGetIntegerValueField. If this key is passed to
  -- CGEventGetDoubleValueField, the fixed-point value is converted to a double
  -- value.
  ScrollWheelEventFixedPtDeltaAxis2 :: EventField Word64

  -- | Key to access an integer field that contains pixel-based scrolling data.
  -- The scrolling data represents the change in vertical position since the last
  -- scrolling event from a Mighty Mouse scroller or a single-wheel mouse
  -- scroller.
  ScrollWheelEventPointDeltaAxis1 :: EventField Word64

  -- | Key to access an integer field that contains pixel-based scrolling data.
  -- The scrolling data represents the change in horizontal position since the
  -- last scrolling event from a Mighty Mouse scroller.
  ScrollWheelEventPointDeltaAxis2 :: EventField Word64

  -- | Key to access an integer field that indicates whether the event should be
  -- ignored by the Inkwell subsystem. If the value is non-zero, the event should be ignored.
  ScrollWheelEventInstantMouser :: EventField Word64

  -- | Key to access an integer field that contains the absolute X coordinate in
  -- tablet space at full tablet resolution.
  TabletEventPointX :: EventField Word64

  -- | Key to access an integer field that contains the absolute Y coordinate in
  -- tablet space at full tablet resolution.
  TabletEventPointY :: EventField Word64

  -- | Key to access an integer field that contains the absolute Z coordinate in
  -- tablet space at full tablet resolution.
  TabletEventPointZ :: EventField Word64

  -- | Key to access an integer field that contains the tablet button state. Bit 0
  -- is the first button, and a set bit represents a closed or pressed button. Up
  -- to 16 buttons are supported.
  TabletEventPointButtons :: EventField Word64

  -- | Key to access a double field that contains the tablet pen pressure.
  -- A value of 0.0 represents no pressure, and 1.0 represents maximum pressure.
  TabletEventPointPressure :: EventField Double

  -- | Key to access a double field that contains the horizontal tablet pen tilt.
  -- A value of 0.0 represents no tilt, and 1.0 represents maximum tilt.
  TabletEventTiltX :: EventField Double

  -- | Key to access a double field that contains the vertical tablet pen tilt.
  -- A value of 0.0 represents no tilt, and 1.0 represents maximum tilt.
  TabletEventTiltY :: EventField Double

  -- | Key to access a double field that contains the tablet pen rotation.
  TabletEventRotation :: EventField Double

  -- | Key to access a double field that contains the tangential pressure on the
  -- device. A value of 0.0 represents no pressure, and 1.0 represents maximum
  -- pressure.
  TabletEventTangentialPressure :: EventField Double

  -- | Key to access an integer field that contains the system-assigned unique
  -- device ID.
  TabletEventDeviceID :: EventField Word64

  -- | Key to access an integer field that contains a vendor-specified value.
  TabletEventVendor1 :: EventField Word64

  -- | Key to access an integer field that contains a vendor-specified value.
  TabletEventVendor2 :: EventField Word64

  -- | Key to access an integer field that contains a vendor-specified value.
  TabletEventVendor3 :: EventField Word64

  -- | Key to access an integer field that contains the vendor-defined ID,
  -- typically the USB vendor ID.
  TabletProximityEventVendorID :: EventField Word64

  -- | Key to access an integer field that contains the vendor-defined tablet ID,
  -- typically the USB product ID.
  TabletProximityEventTabletID :: EventField Word64

  -- | Key to access an integer field that contains the vendor-defined ID of the
  -- pointing device.
  TabletProximityEventPointerID :: EventField Word64

  -- | Key to access an integer field that contains the system-assigned device ID.
  TabletProximityEventDeviceID :: EventField Word64

  -- | Key to access an integer field that contains the system-assigned unique
  -- tablet ID.
  TabletProximityEventSystemTabletID :: EventField Word64

  -- | Key to access an integer field that contains the vendor-assigned pointer
  -- type.
  TabletProximityEventVendorPointerType :: EventField Word64

  -- | Key to access an integer field that contains the vendor-defined pointer
  -- serial number.
  TabletProximityEventVendorPointerSerialNumber :: EventField Word64

  -- | Key to access an integer field that contains the vendor-defined unique ID.
  TabletProximityEventVendorUniqueID :: EventField Word64

  -- | Key to access an integer field that contains the device capabilities mask.
  TabletProximityEventCapabilityMask :: EventField Word64

  -- | Key to access an integer field that contains the pointer type.
  TabletProximityEventPointerType :: EventField Word64

  -- | Key to access an integer field that indicates whether the pen is in
  -- proximity to the tablet. The value is non-zero if the pen is in proximity to
  -- the tablet and zero when leaving the tablet.
  TabletProximityEventEnterProximity :: EventField Word64

  -- | Key to access a field that contains the event target process serial
  -- number. The value is a 64-bit long word.
  EventTargetProcessSerialNumber :: EventField Word64

  -- | Key to access a field that contains the event target Unix process ID.
  EventTargetUnixProcessID :: EventField Word64

  -- | Key to access a field that contains the event source Unix process ID.
  EventSourceUnixProcessID :: EventField Word64

  -- | Key to access a field that contains the event source user-supplied data,
  -- up to 64 bits.
  EventSourceUserData :: EventField Word64

  -- | Key to access a field that contains the event source Unix effective UID.
  EventSourceUserID :: EventField Word64

  -- | Key to access a field that contains the event source Unix effective GID.
  EventSourceGroupID :: EventField Word64

  -- | Key to access a field that contains the event source state ID used to
  -- create this event.
  EventSourceStateID :: EventField Word64

  -- | Key to access an integer field that indicates whether a scrolling event
  -- contains continuous, pixel-based scrolling data. The value is non-zero when
  -- the scrolling data is pixel-based and zero when the scrolling data is
  -- line-based.
  ScrollWheelEventIsContinuous :: EventField Word64

  MouseEventWindowUnderMousePointer :: EventField Word64

  MouseEventWindowUnderMousePointerThatCanHandleThisEvent :: EventField Word64

  ScrollWheelEventMomentumPhase :: EventField Word64

  ScrollWheelEventScrollCount :: EventField Word64

  ScrollWheelEventScrollPhase :: EventField Word64

mouseEventNumber :: Word32
mouseEventNumber = #const kCGMouseEventNumber
mouseEventClickState :: Word32
mouseEventClickState = #const kCGMouseEventClickState
mouseEventPressure :: Word32
mouseEventPressure = #const kCGMouseEventPressure
mouseEventButtonNumber :: Word32
mouseEventButtonNumber = #const kCGMouseEventButtonNumber
mouseEventDeltaX :: Word32
mouseEventDeltaX = #const kCGMouseEventDeltaX
mouseEventDeltaY :: Word32
mouseEventDeltaY = #const kCGMouseEventDeltaY
mouseEventInstantMouser :: Word32
mouseEventInstantMouser = #const kCGMouseEventInstantMouser
mouseEventSubtype :: Word32
mouseEventSubtype = #const kCGMouseEventSubtype
keyboardEventAutorepeat :: Word32
keyboardEventAutorepeat = #const kCGKeyboardEventAutorepeat
keyboardEventKeycode :: Word32
keyboardEventKeycode = #const kCGKeyboardEventKeycode
keyboardEventKeyboardType :: Word32
keyboardEventKeyboardType = #const kCGKeyboardEventKeyboardType
scrollWheelEventDeltaAxis1 :: Word32
scrollWheelEventDeltaAxis1 = #const kCGScrollWheelEventDeltaAxis1
scrollWheelEventDeltaAxis2 :: Word32
scrollWheelEventDeltaAxis2 = #const kCGScrollWheelEventDeltaAxis2
scrollWheelEventDeltaAxis3 :: Word32
scrollWheelEventDeltaAxis3 = #const kCGScrollWheelEventDeltaAxis3
scrollWheelEventFixedPtDeltaAxis1 :: Word32
scrollWheelEventFixedPtDeltaAxis1 = #const kCGScrollWheelEventFixedPtDeltaAxis1
scrollWheelEventFixedPtDeltaAxis2 :: Word32
scrollWheelEventFixedPtDeltaAxis2 = #const kCGScrollWheelEventFixedPtDeltaAxis2
scrollWheelEventFixedPtDeltaAxis3 :: Word32
scrollWheelEventFixedPtDeltaAxis3 = #const kCGScrollWheelEventFixedPtDeltaAxis3
scrollWheelEventPointDeltaAxis1 :: Word32
scrollWheelEventPointDeltaAxis1 = #const kCGScrollWheelEventPointDeltaAxis1
scrollWheelEventPointDeltaAxis2 :: Word32
scrollWheelEventPointDeltaAxis2 = #const kCGScrollWheelEventPointDeltaAxis2
scrollWheelEventPointDeltaAxis3 :: Word32
scrollWheelEventPointDeltaAxis3 = #const kCGScrollWheelEventPointDeltaAxis3
scrollWheelEventInstantMouser :: Word32
scrollWheelEventInstantMouser = #const kCGScrollWheelEventInstantMouser
tabletEventPointX :: Word32
tabletEventPointX = #const kCGTabletEventPointX
tabletEventPointY :: Word32
tabletEventPointY = #const kCGTabletEventPointY
tabletEventPointZ :: Word32
tabletEventPointZ = #const kCGTabletEventPointZ
tabletEventPointButtons :: Word32
tabletEventPointButtons = #const kCGTabletEventPointButtons
tabletEventPointPressure :: Word32
tabletEventPointPressure = #const kCGTabletEventPointPressure
tabletEventTiltX :: Word32
tabletEventTiltX = #const kCGTabletEventTiltX
tabletEventTiltY :: Word32
tabletEventTiltY = #const kCGTabletEventTiltY
tabletEventRotation :: Word32
tabletEventRotation = #const kCGTabletEventRotation
tabletEventTangentialPressure :: Word32
tabletEventTangentialPressure = #const kCGTabletEventTangentialPressure
tabletEventDeviceID :: Word32
tabletEventDeviceID = #const kCGTabletEventDeviceID
tabletEventVendor1 :: Word32
tabletEventVendor1 = #const kCGTabletEventVendor1
tabletEventVendor2 :: Word32
tabletEventVendor2 = #const kCGTabletEventVendor2
tabletEventVendor3 :: Word32
tabletEventVendor3 = #const kCGTabletEventVendor3
tabletProximityEventVendorID :: Word32
tabletProximityEventVendorID = #const kCGTabletProximityEventVendorID
tabletProximityEventTabletID :: Word32
tabletProximityEventTabletID = #const kCGTabletProximityEventTabletID
tabletProximityEventPointerID :: Word32
tabletProximityEventPointerID = #const kCGTabletProximityEventPointerID
tabletProximityEventDeviceID :: Word32
tabletProximityEventDeviceID = #const kCGTabletProximityEventDeviceID
tabletProximityEventSystemTabletID :: Word32
tabletProximityEventSystemTabletID = #const kCGTabletProximityEventSystemTabletID
tabletProximityEventVendorPointerType :: Word32
tabletProximityEventVendorPointerType = #const kCGTabletProximityEventVendorPointerType
tabletProximityEventVendorPointerSerialNumber :: Word32
tabletProximityEventVendorPointerSerialNumber = #const kCGTabletProximityEventVendorPointerSerialNumber
tabletProximityEventVendorUniqueID :: Word32
tabletProximityEventVendorUniqueID = #const kCGTabletProximityEventVendorUniqueID
tabletProximityEventCapabilityMask :: Word32
tabletProximityEventCapabilityMask = #const kCGTabletProximityEventCapabilityMask
tabletProximityEventPointerType :: Word32
tabletProximityEventPointerType = #const kCGTabletProximityEventPointerType
tabletProximityEventEnterProximity :: Word32
tabletProximityEventEnterProximity = #const kCGTabletProximityEventEnterProximity
eventTargetProcessSerialNumber :: Word32
eventTargetProcessSerialNumber = #const kCGEventTargetProcessSerialNumber
eventTargetUnixProcessID :: Word32
eventTargetUnixProcessID = #const kCGEventTargetUnixProcessID
eventSourceUnixProcessID :: Word32
eventSourceUnixProcessID = #const kCGEventSourceUnixProcessID
eventSourceUserData :: Word32
eventSourceUserData = #const kCGEventSourceUserData
eventSourceUserID :: Word32
eventSourceUserID = #const kCGEventSourceUserID
eventSourceGroupID :: Word32
eventSourceGroupID = #const kCGEventSourceGroupID
eventSourceStateID :: Word32
eventSourceStateID = #const kCGEventSourceStateID
scrollWheelEventIsContinuous :: Word32
scrollWheelEventIsContinuous = #const kCGScrollWheelEventIsContinuous
mouseEventWindowUnderMousePointer :: Word32
mouseEventWindowUnderMousePointer = #const kCGMouseEventWindowUnderMousePointer
mouseEventWindowUnderMousePointerThatCanHandleThisEvent :: Word32
mouseEventWindowUnderMousePointerThatCanHandleThisEvent = #const kCGMouseEventWindowUnderMousePointerThatCanHandleThisEvent
scrollWheelEventMomentumPhase :: Word32
scrollWheelEventMomentumPhase = #const kCGScrollWheelEventMomentumPhase
scrollWheelEventScrollCount :: Word32
scrollWheelEventScrollCount = #const kCGScrollWheelEventScrollCount
scrollWheelEventScrollPhase :: Word32
scrollWheelEventScrollPhase = #const kCGScrollWheelEventScrollPhase

toEventFieldCode :: EventField ty -> Word32
toEventFieldCode = \case
  MouseEventNumber -> mouseEventNumber
  MouseEventClickState -> mouseEventClickState
  MouseEventPressure -> mouseEventPressure
  MouseEventButtonNumber -> mouseEventButtonNumber
  MouseEventDeltaX -> mouseEventDeltaX
  MouseEventDeltaY -> mouseEventDeltaY
  MouseEventInstantMouser -> mouseEventInstantMouser
  MouseEventSubtype -> mouseEventSubtype
  KeyboardEventAutorepeat -> keyboardEventAutorepeat
  KeyboardEventKeycode -> keyboardEventKeycode
  KeyboardEventKeyboardType -> keyboardEventKeyboardType
  ScrollWheelEventDeltaAxis1 -> scrollWheelEventDeltaAxis1
  ScrollWheelEventDeltaAxis2 -> scrollWheelEventDeltaAxis2
  ScrollWheelEventFixedPtDeltaAxis1 -> scrollWheelEventFixedPtDeltaAxis1
  ScrollWheelEventFixedPtDeltaAxis2 -> scrollWheelEventFixedPtDeltaAxis2
  ScrollWheelEventPointDeltaAxis1 -> scrollWheelEventPointDeltaAxis1
  ScrollWheelEventPointDeltaAxis2 -> scrollWheelEventPointDeltaAxis2
  ScrollWheelEventInstantMouser -> scrollWheelEventInstantMouser
  TabletEventPointX -> tabletEventPointX
  TabletEventPointY -> tabletEventPointY
  TabletEventPointZ -> tabletEventPointZ
  TabletEventPointButtons -> tabletEventPointButtons
  TabletEventPointPressure -> tabletEventPointPressure
  TabletEventTiltX -> tabletEventTiltX
  TabletEventTiltY -> tabletEventTiltY
  TabletEventRotation -> tabletEventRotation
  TabletEventTangentialPressure -> tabletEventTangentialPressure
  TabletEventDeviceID -> tabletEventDeviceID
  TabletEventVendor1 -> tabletEventVendor1
  TabletEventVendor2 -> tabletEventVendor2
  TabletEventVendor3 -> tabletEventVendor3
  TabletProximityEventVendorID -> tabletProximityEventVendorID
  TabletProximityEventTabletID -> tabletProximityEventTabletID
  TabletProximityEventPointerID -> tabletProximityEventPointerID
  TabletProximityEventDeviceID -> tabletProximityEventDeviceID
  TabletProximityEventSystemTabletID -> tabletProximityEventSystemTabletID
  TabletProximityEventVendorPointerType -> tabletProximityEventVendorPointerType
  TabletProximityEventVendorPointerSerialNumber -> tabletProximityEventVendorPointerSerialNumber
  TabletProximityEventVendorUniqueID -> tabletProximityEventVendorUniqueID
  TabletProximityEventCapabilityMask -> tabletProximityEventCapabilityMask
  TabletProximityEventPointerType -> tabletProximityEventPointerType
  TabletProximityEventEnterProximity -> tabletProximityEventEnterProximity
  EventTargetProcessSerialNumber -> eventTargetProcessSerialNumber
  EventTargetUnixProcessID -> eventTargetUnixProcessID
  EventSourceUnixProcessID -> eventSourceUnixProcessID
  EventSourceUserData -> eventSourceUserData
  EventSourceUserID -> eventSourceUserID
  EventSourceGroupID -> eventSourceGroupID
  EventSourceStateID -> eventSourceStateID
  ScrollWheelEventIsContinuous -> scrollWheelEventIsContinuous
  MouseEventWindowUnderMousePointer -> mouseEventWindowUnderMousePointer
  MouseEventWindowUnderMousePointerThatCanHandleThisEvent ->
    mouseEventWindowUnderMousePointerThatCanHandleThisEvent
  ScrollWheelEventMomentumPhase -> scrollWheelEventMomentumPhase
  ScrollWheelEventScrollCount -> scrollWheelEventScrollCount
  ScrollWheelEventScrollPhase -> scrollWheelEventScrollPhase

-- decideFieldType :: EventField ty
--                 -> Either (Dict (ty ~ Word64)) (Dict (ty ~ Double))

decideFieldType
  :: (ty ~ Word64 => a) -> (ty ~ Double => a) -> EventField ty -> a
decideFieldType f g = \case
  MouseEventNumber -> f
  MouseEventClickState -> f
  MouseEventPressure -> g
  MouseEventButtonNumber -> f
  MouseEventDeltaX -> f
  MouseEventDeltaY -> f
  MouseEventInstantMouser -> f
  MouseEventSubtype -> f
  KeyboardEventAutorepeat -> f
  KeyboardEventKeycode -> f
  KeyboardEventKeyboardType -> f
  ScrollWheelEventDeltaAxis1 -> g
  ScrollWheelEventDeltaAxis2 -> g
  ScrollWheelEventFixedPtDeltaAxis1 -> f
  ScrollWheelEventFixedPtDeltaAxis2 -> f
  ScrollWheelEventPointDeltaAxis1 -> f
  ScrollWheelEventPointDeltaAxis2 -> f
  ScrollWheelEventInstantMouser -> f
  TabletEventPointX -> f
  TabletEventPointY -> f
  TabletEventPointZ -> f
  TabletEventPointButtons -> f
  TabletEventPointPressure -> g
  TabletEventTiltX -> g
  TabletEventTiltY -> g
  TabletEventRotation -> g
  TabletEventTangentialPressure -> g
  TabletEventDeviceID -> f
  TabletEventVendor1 -> f
  TabletEventVendor2 -> f
  TabletEventVendor3 -> f
  TabletProximityEventVendorID -> f
  TabletProximityEventTabletID -> f
  TabletProximityEventPointerID -> f
  TabletProximityEventDeviceID -> f
  TabletProximityEventSystemTabletID -> f
  TabletProximityEventVendorPointerType -> f
  TabletProximityEventVendorPointerSerialNumber -> f
  TabletProximityEventVendorUniqueID -> f
  TabletProximityEventCapabilityMask -> f
  TabletProximityEventPointerType -> f
  TabletProximityEventEnterProximity -> f
  EventTargetProcessSerialNumber -> f
  EventTargetUnixProcessID -> f
  EventSourceUnixProcessID -> f
  EventSourceUserData -> f
  EventSourceUserID -> f
  EventSourceGroupID -> f
  EventSourceStateID -> f
  ScrollWheelEventIsContinuous -> f
  MouseEventWindowUnderMousePointer -> f
  MouseEventWindowUnderMousePointerThatCanHandleThisEvent -> f
  ScrollWheelEventMomentumPhase -> f
  ScrollWheelEventScrollCount -> f
  ScrollWheelEventScrollPhase -> f
