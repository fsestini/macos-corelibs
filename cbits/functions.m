#include <Carbon/Carbon.h>

CGError CGWarpMouseCursorPosition_(CGPoint *pos) {
  return CGWarpMouseCursorPosition(*pos);
}

CGEventRef CGEventCreateMouseEvent_(
  CGEventSourceRef source,
  CGEventType mouseType,
  CGPoint *position,
  CGMouseButton mouseButton) {
  return CGEventCreateMouseEvent(source, mouseType, *position, mouseButton);
}

typedef int CGSConnectionID;

extern CGSConnectionID _CGSDefaultConnection(void);
#define CGSDefaultConnection _CGSDefaultConnection()

extern CFStringRef CGSCopyManagedDisplayForWindow(
  const CGSConnectionID Connection, uint32_t WindowId);

void cgDisplayBounds_(uint32_t d_id, CGRect *rect)
{ *rect = CGDisplayBounds(d_id); }

CFStringRef display_for_window(uint32_t window) {
  return CGSCopyManagedDisplayForWindow(CGSDefaultConnection, window);
}

bool get_isback(ProcessSerialNumber *psn) {
  ProcessInfoRec ProcessInfo = {};
  ProcessInfo.processInfoLength = sizeof(ProcessInfoRec);
  GetProcessInformation(psn, &ProcessInfo);
  return (ProcessInfo.processMode & modeOnlyBackground) != 0;
}

typedef OSStatus (carbon_event_callback_t)
  (EventHandlerCallRef HandlerCallRef, EventRef Event, void *Refcon);

EventHandlerUPP handler_upp(carbon_event_callback_t *callb) {
  return NewEventHandlerUPP(callb); }

CGEventMask event_type_to_mask_bit(CGEventType ety) {
  return CGEventMaskBit(ety);
}
