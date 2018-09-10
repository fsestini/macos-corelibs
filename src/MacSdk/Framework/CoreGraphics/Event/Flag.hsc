{-|

Constants that indicate the modifier key state at the time an event is created,
as well as other event-related states.

-}

module MacSdk.Framework.CoreGraphics.Event.Flag where

#include <Carbon/Carbon.h>

import Data.Word
import Data.Bits
import Data.Maybe (catMaybes)

type CGEventFlags = Word64

data EventFlag
  -- | Indicates that the Caps Lock key is down for a keyboard, mouse, or
  -- flag-changed event.
  = EventFlagAlphaShift
  -- | Indicates that the Shift key is down for a keyboard, mouse,
  -- or flag-changed event.
  | EventFlagShift
  | EventFlagLeftShift
  | EventFlagRightShift
  -- | Indicates that the Control key is down for a keyboard, mouse,
  -- or flag-changed event.
  | EventFlagControl
  | EventFlagLeftControl
  | EventFlagRightControl
  -- | Indicates that the Alt or Option key is down for a keyboard, mouse, or
  -- flag-changed event.
  | EventFlagAlternate
  | EventFlagLeftAlternate
  | EventFlagRightAlternate
  -- | Indicates that the Command key is down for a keyboard, mouse,
  -- or flag-changed event.
  | EventFlagCommand
  | EventFlagLeftCommand
  | EventFlagRightCommand
  -- | Indicates that the Help modifier key is down for a keyboard, mouse, or
  -- flag-changed event. This key is not present on most keyboards, and is
  -- different than the Help key found in the same row as Home and Page Up.
  | EventFlagHelp
  -- | Indicates that the Fn (Function) key is down for a keyboard, mouse,
  -- or flag-changed event. This key is found primarily on laptop keyboards.
  | EventFlagSecondaryFn
  -- | Identifies key events from the numeric keypad area on extended keyboards.
  | EventFlagNumericPad
  -- | Indicates that mouse and pen movement events are not being coalesced.
  | EventFlagNonCoalesced

toCGEventFlags :: [EventFlag] -> CGEventFlags
toCGEventFlags = foldr (.|.) 0 . fmap toCGEventFlag

fromCGEventFlags :: CGEventFlags -> [EventFlag]
fromCGEventFlags flags =
  catMaybes . fmap (\f -> if (toCGEventFlag f .&. flags) == toCGEventFlag f
                            then Just f
                            else Nothing) $
  [ EventFlagAlphaShift
  , EventFlagShift
  , EventFlagLeftShift
  , EventFlagRightShift
  , EventFlagControl
  , EventFlagLeftControl
  , EventFlagRightControl
  , EventFlagAlternate
  , EventFlagLeftAlternate
  , EventFlagRightAlternate
  , EventFlagCommand
  , EventFlagLeftCommand
  , EventFlagRightCommand
  , EventFlagHelp
  , EventFlagSecondaryFn
  , EventFlagNumericPad
  , EventFlagNonCoalesced
  ]

toCGEventFlag :: EventFlag -> CGEventFlags
toCGEventFlag = \case
  EventFlagAlphaShift -> eventFlagMaskAlphaShift
  EventFlagShift -> eventFlagMaskShift
  EventFlagLeftShift -> 0x00000002
  EventFlagRightShift -> 0x00000004
  EventFlagControl -> eventFlagMaskControl
  EventFlagLeftControl -> 0x00000001
  EventFlagRightControl -> 0x00002000
  EventFlagAlternate -> eventFlagMaskAlternate
  EventFlagLeftAlternate -> 0x00000020
  EventFlagRightAlternate -> 0x00000040
  EventFlagCommand -> eventFlagMaskCommand
  EventFlagLeftCommand -> 0x00000008
  EventFlagRightCommand -> 0x00000010
  EventFlagHelp -> eventFlagMaskHelp
  EventFlagSecondaryFn -> eventFlagMaskSecondaryFn
  EventFlagNumericPad -> eventFlagMaskNumericPad
  EventFlagNonCoalesced -> eventFlagMaskNonCoalesced

eventFlagMaskAlphaShift :: Word64
eventFlagMaskAlphaShift = #const kCGEventFlagMaskAlphaShift

eventFlagMaskShift :: Word64
eventFlagMaskShift = #const kCGEventFlagMaskShift

eventFlagMaskControl :: Word64
eventFlagMaskControl = #const kCGEventFlagMaskControl

eventFlagMaskAlternate :: Word64
eventFlagMaskAlternate = #const kCGEventFlagMaskAlternate

eventFlagMaskCommand :: Word64
eventFlagMaskCommand = #const kCGEventFlagMaskCommand

eventFlagMaskHelp :: Word64
eventFlagMaskHelp = #const kCGEventFlagMaskHelp

eventFlagMaskSecondaryFn :: Word64
eventFlagMaskSecondaryFn = #const kCGEventFlagMaskSecondaryFn

eventFlagMaskNumericPad :: Word64
eventFlagMaskNumericPad = #const kCGEventFlagMaskNumericPad

eventFlagMaskNonCoalesced :: Word64
eventFlagMaskNonCoalesced = #const kCGEventFlagMaskNonCoalesced
