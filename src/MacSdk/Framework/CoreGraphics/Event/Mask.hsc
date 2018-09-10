-- | Constants and ADTs to define a mask that identifies the set of Quartz
-- events to be observed in an event tap.

module MacSdk.Framework.CoreGraphics.Event.Mask where

#include <Carbon/Carbon.h>

import Data.Word
import Data.Bits
import MacSdk.Framework.CoreGraphics.Event.Type

foreign import ccall unsafe event_type_to_mask_bit :: Word32 -> Word64

type CGEventMask = Word64
newtype EventMask = EventMask { getEventMask :: Word64 }

instance Semigroup EventMask where
  EventMask m1 <> EventMask m2 = EventMask (m1 .|. m2)

instance Monoid EventMask where
  mempty = EventMask 0
  mappend = (<>)

data QuartzEvents = AllEvents | EventList [EventType]

allEvents :: Word64
allEvents = #const kCGEventMaskForAllEvents

toMask :: QuartzEvents -> EventMask
toMask AllEvents = EventMask allEvents
toMask (EventList tys) =
  mconcat (fmap (EventMask . event_type_to_mask_bit . toEventTypeCode) tys)
