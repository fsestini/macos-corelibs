-- | Constants and types that specify possible tapping points for events.

module MacSdk.Framework.CoreGraphics.Event.Location where

#include <Carbon/Carbon.h>

import Foreign.C.Types (CInt(..))

type CGEventTapLocation = CInt

data EventTapLocation
  = HIDEventTap
  -- ^ Specifies that an event tap is placed at the point where HID system
  -- events enter the window server.
  | SessionEventTap
  -- ^ Specifies that an event tap is placed at the point where HID system and
  -- remote control events enter a login session.
  | AnnotatedSessionEventTap
  -- ^ Specifies that an event tap is placed at the point where session events
  -- have been annotated to flow to an application.

toCGEventTapLocation :: EventTapLocation -> CGEventTapLocation
toCGEventTapLocation = \case
  HIDEventTap -> (#const kCGHIDEventTap)
  SessionEventTap -> (#const kCGSessionEventTap)
  AnnotatedSessionEventTap -> (#const kCGAnnotatedSessionEventTap)
