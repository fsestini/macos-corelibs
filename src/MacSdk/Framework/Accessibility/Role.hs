{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

-- | Constants and ADTs defining the values an accessibility object’s role
-- attribute can have.

module MacSdk.Framework.Accessibility.Role where

import MacSdk.Framework.CoreFoundation
import Control.Monad.Trans.Maybe (MaybeT(..))
import Control.Monad (msum)
import Data.Maybe (fromMaybe)

data Role
  = ApplicationRole
  -- ^ An application.
  | SystemWideRole
  -- ^ The system-wide accessibility object.
  | WindowRole
  -- ^ A window.
  | SheetRole
  -- ^ A sheet.
  | DrawerRole
  -- ^ A drawer.
  | GrowAreaRole
  -- ^ A grow control.
  | ImageRole
  -- ^ An image.
  | UnknownRole
  -- ^ Generic role value for an unknown accessibility object.
  | ButtonRole
  -- ^ A button.
  | RadioButtonRole
  -- ^ A radio button.
  | CheckBoxRole
  -- ^ A check box.
  | PopUpButtonRole
  -- ^ A pop-up button.
  | MenuButtonRole
  -- ^ A menu button.
  | TabGroupRole
  -- ^ A tab view.
  | TableRole
  -- ^ A table.
  | ColumnRole
  -- ^ A column.
  | RowRole
  -- ^ A row.
  | OutlineRole
  -- ^ An accessibility object that displays row-based, hierarchically
  -- structured data, such as the list view in a Finder window.
  | BrowserRole
  -- ^ An accessibility object that displays column-based, hierarchically
  -- structured data, such as the column view in a Finder window.
  | ScrollAreaRole
  -- ^ An accessibility object that displays data managed by scrolling controls.
  | ScrollBarRole
  -- ^ A scroll bar control.
  | RadioGroupRole
  -- ^ A set of radio buttons.
  | ListRole
  -- ^ A list view.
  | GroupRole
  -- ^ A group box. This role can also be used to group other views without any
  -- visual indication of the grouping.
  | ValueIndicatorRole
  -- ^ A control that indicates the value of an accessibility object, such as
  -- the scroller of a scroll bar control
  | ComboBoxRole
  -- ^ A combo box control.
  | SliderRole
  -- ^ A slider control.
  | IncrementorRole
  -- ^ A stepper control (also known as the “little arrows”).
  | BusyIndicatorRole
  -- ^ An asynchronous progress indicator.
  | ProgressIndicatorRole
  -- ^ A determinate or indeterminate progress indicator.
  | RelevanceIndicatorRole
  -- ^ A relevance indicator.
  | ToolbarRole
  -- ^ A toolbar.
  | DisclosureTriangleRole
  -- ^ A disclosure triangle control.
  | TextFieldRole
  -- ^ A text field.
  | TextAreaRole
  -- ^ The editable text area in a control or window.
  | StaticTextRole
  -- ^ A string of static text displayed in a window that is not part of any
  -- control.
  | MenuBarRole
  -- ^ A menu bar.
  | MenuBarItemRole
  -- ^ A menu bar item.
  | MenuRole
  -- ^ A menu.
  | MenuItemRole
  -- ^ A menu item.
  | SplitGroupRole
  -- ^ A split view.
  | SplitterRole
  -- ^ A splitter bar control.
  | ColorWellRole
  -- ^ A color well.
  | TimeFieldRole
  -- ^ A field that displays time.
  | DateFieldRole
  -- ^ A field that displays dates.
  | HelpTagRole
  -- ^ A help tag.
  | MatteRole
  | DockItemRole
  -- ^ An icon that represents an item in the Dock.
  | OtherRole String
  deriving (Eq, Show)

foreign import ccall unsafe kAXApplicationRole_ :: IO CFStringRef
foreign import ccall unsafe kAXSystemWideRole_ :: IO CFStringRef
foreign import ccall unsafe kAXWindowRole_ :: IO CFStringRef
foreign import ccall unsafe kAXSheetRole_ :: IO CFStringRef
foreign import ccall unsafe kAXDrawerRole_ :: IO CFStringRef
foreign import ccall unsafe kAXGrowAreaRole_ :: IO CFStringRef
foreign import ccall unsafe kAXImageRole_ :: IO CFStringRef
foreign import ccall unsafe kAXUnknownRole_ :: IO CFStringRef
foreign import ccall unsafe kAXButtonRole_ :: IO CFStringRef
foreign import ccall unsafe kAXRadioButtonRole_ :: IO CFStringRef
foreign import ccall unsafe kAXCheckBoxRole_ :: IO CFStringRef
foreign import ccall unsafe kAXPopUpButtonRole_ :: IO CFStringRef
foreign import ccall unsafe kAXMenuButtonRole_ :: IO CFStringRef
foreign import ccall unsafe kAXTabGroupRole_ :: IO CFStringRef
foreign import ccall unsafe kAXTableRole_ :: IO CFStringRef
foreign import ccall unsafe kAXColumnRole_ :: IO CFStringRef
foreign import ccall unsafe kAXRowRole_ :: IO CFStringRef
foreign import ccall unsafe kAXOutlineRole_ :: IO CFStringRef
foreign import ccall unsafe kAXBrowserRole_ :: IO CFStringRef
foreign import ccall unsafe kAXScrollAreaRole_ :: IO CFStringRef
foreign import ccall unsafe kAXScrollBarRole_ :: IO CFStringRef
foreign import ccall unsafe kAXRadioGroupRole_ :: IO CFStringRef
foreign import ccall unsafe kAXListRole_ :: IO CFStringRef
foreign import ccall unsafe kAXGroupRole_ :: IO CFStringRef
foreign import ccall unsafe kAXValueIndicatorRole_ :: IO CFStringRef
foreign import ccall unsafe kAXComboBoxRole_ :: IO CFStringRef
foreign import ccall unsafe kAXSliderRole_ :: IO CFStringRef
foreign import ccall unsafe kAXIncrementorRole_ :: IO CFStringRef
foreign import ccall unsafe kAXBusyIndicatorRole_ :: IO CFStringRef
foreign import ccall unsafe kAXProgressIndicatorRole_ :: IO CFStringRef
foreign import ccall unsafe kAXRelevanceIndicatorRole_ :: IO CFStringRef
foreign import ccall unsafe kAXToolbarRole_ :: IO CFStringRef
foreign import ccall unsafe kAXDisclosureTriangleRole_ :: IO CFStringRef
foreign import ccall unsafe kAXTextFieldRole_ :: IO CFStringRef
foreign import ccall unsafe kAXTextAreaRole_ :: IO CFStringRef
foreign import ccall unsafe kAXStaticTextRole_ :: IO CFStringRef
foreign import ccall unsafe kAXMenuBarRole_ :: IO CFStringRef
foreign import ccall unsafe kAXMenuBarItemRole_ :: IO CFStringRef
foreign import ccall unsafe kAXMenuRole_ :: IO CFStringRef
foreign import ccall unsafe kAXMenuItemRole_ :: IO CFStringRef
foreign import ccall unsafe kAXSplitGroupRole_ :: IO CFStringRef
foreign import ccall unsafe kAXSplitterRole_ :: IO CFStringRef
foreign import ccall unsafe kAXColorWellRole_ :: IO CFStringRef
foreign import ccall unsafe kAXTimeFieldRole_ :: IO CFStringRef
foreign import ccall unsafe kAXDateFieldRole_ :: IO CFStringRef
foreign import ccall unsafe kAXHelpTagRole_ :: IO CFStringRef
foreign import ccall unsafe kAXMatteRole_ :: IO CFStringRef
foreign import ccall unsafe kAXDockItemRole_ :: IO CFStringRef

toRoleCFStringRef :: Role -> IO CFStringRef
toRoleCFStringRef = \case
  ApplicationRole -> kAXApplicationRole_
  SystemWideRole -> kAXSystemWideRole_
  WindowRole -> kAXWindowRole_
  SheetRole -> kAXSheetRole_
  DrawerRole -> kAXDrawerRole_
  GrowAreaRole -> kAXGrowAreaRole_
  ImageRole -> kAXImageRole_
  UnknownRole -> kAXUnknownRole_
  ButtonRole -> kAXButtonRole_
  RadioButtonRole -> kAXRadioButtonRole_
  CheckBoxRole -> kAXCheckBoxRole_
  PopUpButtonRole -> kAXPopUpButtonRole_
  MenuButtonRole -> kAXMenuButtonRole_
  TabGroupRole -> kAXTabGroupRole_
  TableRole -> kAXTableRole_
  ColumnRole -> kAXColumnRole_
  RowRole -> kAXRowRole_
  OutlineRole -> kAXOutlineRole_
  BrowserRole -> kAXBrowserRole_
  ScrollAreaRole -> kAXScrollAreaRole_
  ScrollBarRole -> kAXScrollBarRole_
  RadioGroupRole -> kAXRadioGroupRole_
  ListRole -> kAXListRole_
  GroupRole -> kAXGroupRole_
  ValueIndicatorRole -> kAXValueIndicatorRole_
  ComboBoxRole -> kAXComboBoxRole_
  SliderRole -> kAXSliderRole_
  IncrementorRole -> kAXIncrementorRole_
  BusyIndicatorRole -> kAXBusyIndicatorRole_
  ProgressIndicatorRole -> kAXProgressIndicatorRole_
  RelevanceIndicatorRole -> kAXRelevanceIndicatorRole_
  ToolbarRole -> kAXToolbarRole_
  DisclosureTriangleRole -> kAXDisclosureTriangleRole_
  TextFieldRole -> kAXTextFieldRole_
  TextAreaRole -> kAXTextAreaRole_
  StaticTextRole -> kAXStaticTextRole_
  MenuBarRole -> kAXMenuBarRole_
  MenuBarItemRole -> kAXMenuBarItemRole_
  MenuRole -> kAXMenuRole_
  MenuItemRole -> kAXMenuItemRole_
  SplitGroupRole -> kAXSplitGroupRole_
  SplitterRole -> kAXSplitterRole_
  ColorWellRole -> kAXColorWellRole_
  TimeFieldRole -> kAXTimeFieldRole_
  DateFieldRole -> kAXDateFieldRole_
  HelpTagRole -> kAXHelpTagRole_
  MatteRole -> kAXMatteRole_
  DockItemRole -> kAXDockItemRole_
  OtherRole str -> fromString CFStringEncodingASCII str >>= flip withCFPtr pure

fromRoleCFString :: CFString -> IO Role
fromRoleCFString roleString = withCFPtr roleString $ \role -> do
  Just str' <- toString CFStringEncodingASCII roleString
  fmap (fromMaybe (OtherRole str')) . runMaybeT . msum . fmap MaybeT $
    [ kAXApplicationRole_ >>= \str -> cfEqual str role >>= \b ->
        pure (if b then Just ApplicationRole else Nothing)
    , kAXSystemWideRole_ >>= \str -> cfEqual str role >>= \b ->
        pure (if b then Just SystemWideRole else Nothing)
    , kAXWindowRole_ >>= \str -> cfEqual str role >>= \b ->
        pure (if b then Just WindowRole else Nothing)
    , kAXSheetRole_ >>= \str -> cfEqual str role >>= \b ->
        pure (if b then Just SheetRole else Nothing)
    , kAXDrawerRole_ >>= \str -> cfEqual str role >>= \b ->
        pure (if b then Just DrawerRole else Nothing)
    , kAXGrowAreaRole_ >>= \str -> cfEqual str role >>= \b ->
        pure (if b then Just GrowAreaRole else Nothing)
    , kAXImageRole_ >>= \str -> cfEqual str role >>= \b ->
        pure (if b then Just ImageRole else Nothing)
    , kAXUnknownRole_ >>= \str -> cfEqual str role >>= \b ->
        pure (if b then Just UnknownRole else Nothing)
    , kAXButtonRole_ >>= \str -> cfEqual str role >>= \b ->
        pure (if b then Just ButtonRole else Nothing)
    , kAXRadioButtonRole_ >>= \str -> cfEqual str role >>= \b ->
        pure (if b then Just RadioButtonRole else Nothing)
    , kAXCheckBoxRole_ >>= \str -> cfEqual str role >>= \b ->
        pure (if b then Just CheckBoxRole else Nothing)
    , kAXPopUpButtonRole_ >>= \str -> cfEqual str role >>= \b ->
        pure (if b then Just PopUpButtonRole else Nothing)
    , kAXMenuButtonRole_ >>= \str -> cfEqual str role >>= \b ->
        pure (if b then Just MenuButtonRole else Nothing)
    , kAXTabGroupRole_ >>= \str -> cfEqual str role >>= \b ->
        pure (if b then Just TabGroupRole else Nothing)
    , kAXTableRole_ >>= \str -> cfEqual str role >>= \b ->
        pure (if b then Just TableRole else Nothing)
    , kAXColumnRole_ >>= \str -> cfEqual str role >>= \b ->
        pure (if b then Just ColumnRole else Nothing)
    , kAXRowRole_ >>= \str -> cfEqual str role >>= \b ->
        pure (if b then Just RowRole else Nothing)
    , kAXOutlineRole_ >>= \str -> cfEqual str role >>= \b ->
        pure (if b then Just OutlineRole else Nothing)
    , kAXBrowserRole_ >>= \str -> cfEqual str role >>= \b ->
        pure (if b then Just BrowserRole else Nothing)
    , kAXScrollAreaRole_ >>= \str -> cfEqual str role >>= \b ->
        pure (if b then Just ScrollAreaRole else Nothing)
    , kAXScrollBarRole_ >>= \str -> cfEqual str role >>= \b ->
        pure (if b then Just ScrollBarRole else Nothing)
    , kAXRadioGroupRole_ >>= \str -> cfEqual str role >>= \b ->
        pure (if b then Just RadioGroupRole else Nothing)
    , kAXListRole_ >>= \str -> cfEqual str role >>= \b ->
        pure (if b then Just ListRole else Nothing)
    , kAXGroupRole_ >>= \str -> cfEqual str role >>= \b ->
        pure (if b then Just GroupRole else Nothing)
    , kAXValueIndicatorRole_ >>= \str -> cfEqual str role >>= \b ->
        pure (if b then Just ValueIndicatorRole else Nothing)
    , kAXComboBoxRole_ >>= \str -> cfEqual str role >>= \b ->
        pure (if b then Just ComboBoxRole else Nothing)
    , kAXSliderRole_ >>= \str -> cfEqual str role >>= \b ->
        pure (if b then Just SliderRole else Nothing)
    , kAXIncrementorRole_ >>= \str -> cfEqual str role >>= \b ->
        pure (if b then Just IncrementorRole else Nothing)
    , kAXBusyIndicatorRole_ >>= \str -> cfEqual str role >>= \b ->
        pure (if b then Just BusyIndicatorRole else Nothing)
    , kAXProgressIndicatorRole_ >>= \str -> cfEqual str role >>= \b ->
        pure (if b then Just ProgressIndicatorRole else Nothing)
    , kAXRelevanceIndicatorRole_ >>= \str -> cfEqual str role >>= \b ->
        pure (if b then Just RelevanceIndicatorRole else Nothing)
    , kAXToolbarRole_ >>= \str -> cfEqual str role >>= \b ->
        pure (if b then Just ToolbarRole else Nothing)
    , kAXDisclosureTriangleRole_ >>= \str -> cfEqual str role >>= \b ->
        pure (if b then Just DisclosureTriangleRole else Nothing)
    , kAXTextFieldRole_ >>= \str -> cfEqual str role >>= \b ->
        pure (if b then Just TextFieldRole else Nothing)
    , kAXTextAreaRole_ >>= \str -> cfEqual str role >>= \b ->
        pure (if b then Just TextAreaRole else Nothing)
    , kAXStaticTextRole_ >>= \str -> cfEqual str role >>= \b ->
        pure (if b then Just StaticTextRole else Nothing)
    , kAXMenuBarRole_ >>= \str -> cfEqual str role >>= \b ->
        pure (if b then Just MenuBarRole else Nothing)
    , kAXMenuBarItemRole_ >>= \str -> cfEqual str role >>= \b ->
        pure (if b then Just MenuBarItemRole else Nothing)
    , kAXMenuRole_ >>= \str -> cfEqual str role >>= \b ->
        pure (if b then Just MenuRole else Nothing)
    , kAXMenuItemRole_ >>= \str -> cfEqual str role >>= \b ->
        pure (if b then Just MenuItemRole else Nothing)
    , kAXSplitGroupRole_ >>= \str -> cfEqual str role >>= \b ->
        pure (if b then Just SplitGroupRole else Nothing)
    , kAXSplitterRole_ >>= \str -> cfEqual str role >>= \b ->
        pure (if b then Just SplitterRole else Nothing)
    , kAXColorWellRole_ >>= \str -> cfEqual str role >>= \b ->
        pure (if b then Just ColorWellRole else Nothing)
    , kAXTimeFieldRole_ >>= \str -> cfEqual str role >>= \b ->
        pure (if b then Just TimeFieldRole else Nothing)
    , kAXDateFieldRole_ >>= \str -> cfEqual str role >>= \b ->
        pure (if b then Just DateFieldRole else Nothing)
    , kAXHelpTagRole_ >>= \str -> cfEqual str role >>= \b ->
        pure (if b then Just HelpTagRole else Nothing)
    , kAXMatteRole_ >>= \str -> cfEqual str role >>= \b ->
        pure (if b then Just MatteRole else Nothing)
    , kAXDockItemRole_ >>= \str -> cfEqual str role >>= \b ->
        pure (if b then Just DockItemRole else Nothing)
    ]

data Subrole
  = CloseButtonSubrole
  -- ^ A close button (that is, the red button in a window’s title bar that
  -- closes the window).
  | MinimizeButtonSubrole
  -- ^ A minimize button (that is, the yellow button in a window’s title bar
  -- that minimizes the window into the Dock).
  | ZoomButtonSubrole
  -- ^ A zoom button (that is, the green button in a window’s title bar that
  -- adjusts the window’s size).
  | ToolbarButtonSubrole
  -- ^ A toolbar button (that is, the button in a window’s title bar that hides
  -- and reveals the toolbar).
  | SecureTextFieldSubrole
  -- ^ A text field intended to contain sensitive data and that displays the
  -- user’s input as a series of bullets.
  | TableRowSubrole
  -- ^ A row in a table.
  | OutlineRowSubrole
  -- ^ A row in an outline view (see OutlineRole for a description of an outline
  -- view).
  | UnknownSubrole
  | StandardWindowSubrole
  -- ^ A standard window that includes a title bar (that is, not an inspector
  -- window or a sheet).
  | DialogSubrole
  -- ^ A dialog window, such as an alert.
  | SystemDialogSubrole
  -- ^ A system-generated dialog window that floats on the top layer, regardless
  -- of which application is frontmost. Use this subrole only when a dialog or
  -- alert applies to the system as a whole, such as a shutdown dialog.
  | FloatingWindowSubrole
  -- ^ A utility window.
  | SystemFloatingWindowSubrole
  -- ^ A system-generated utility window.
  | IncrementArrowSubrole
  -- ^ The up arrow of a scroll bar.
  | DecrementArrowSubrole
  -- ^ The down arrow of a scroll bar.
  | IncrementPageSubrole
  -- ^ The increment area in the scroll track of a scroll bar.
  | DecrementPageSubrole
  -- ^ The decrement area in the scroll track of a scroll bar.
  | SortButtonSubrole
  -- ^ A column heading button in a list or column view.
  | SearchFieldSubrole
  -- ^ A search field.
  | ApplicationDockItemSubrole
  -- ^ An icon in the Dock that represents an application.
  | DocumentDockItemSubrole
  -- ^ An icon in the Dock that represents a document.
  | FolderDockItemSubrole
  -- ^ An icon in the Dock that represents a folder.
  | MinimizedWindowDockItemSubrole
  -- ^ An icon in the Dock that represents a minimized window.
  | URLDockItemSubrole
  -- ^ An icon in the Dock that represents a URL.
  | DockExtraDockItemSubrole
  -- ^ An icon in the Dock that represents a Dock Extra.
  | TrashDockItemSubrole
  -- ^ The icon in the Dock that represents the Trash.
  | ProcessSwitcherListSubrole
  -- ^ The display of running applications (processes) that appears when a user
  -- presses Command-Tab.
  | SubroleOther String -- CFString
  deriving (Eq, Show)

foreign import ccall unsafe kAXCloseButtonSubrole_ :: IO CFStringRef
foreign import ccall unsafe kAXMinimizeButtonSubrole_ :: IO CFStringRef
foreign import ccall unsafe kAXZoomButtonSubrole_ :: IO CFStringRef
foreign import ccall unsafe kAXToolbarButtonSubrole_ :: IO CFStringRef
foreign import ccall unsafe kAXSecureTextFieldSubrole_ :: IO CFStringRef
foreign import ccall unsafe kAXTableRowSubrole_ :: IO CFStringRef
foreign import ccall unsafe kAXOutlineRowSubrole_ :: IO CFStringRef
foreign import ccall unsafe kAXUnknownSubrole_ :: IO CFStringRef
foreign import ccall unsafe kAXStandardWindowSubrole_ :: IO CFStringRef
foreign import ccall unsafe kAXDialogSubrole_ :: IO CFStringRef
foreign import ccall unsafe kAXSystemDialogSubrole_ :: IO CFStringRef
foreign import ccall unsafe kAXFloatingWindowSubrole_ :: IO CFStringRef
foreign import ccall unsafe kAXSystemFloatingWindowSubrole_ :: IO CFStringRef
foreign import ccall unsafe kAXIncrementArrowSubrole_ :: IO CFStringRef
foreign import ccall unsafe kAXDecrementArrowSubrole_ :: IO CFStringRef
foreign import ccall unsafe kAXIncrementPageSubrole_ :: IO CFStringRef
foreign import ccall unsafe kAXDecrementPageSubrole_ :: IO CFStringRef
foreign import ccall unsafe kAXSortButtonSubrole_ :: IO CFStringRef
foreign import ccall unsafe kAXSearchFieldSubrole_ :: IO CFStringRef
foreign import ccall unsafe kAXApplicationDockItemSubrole_ :: IO CFStringRef
foreign import ccall unsafe kAXDocumentDockItemSubrole_ :: IO CFStringRef
foreign import ccall unsafe kAXFolderDockItemSubrole_ :: IO CFStringRef
foreign import ccall unsafe kAXMinimizedWindowDockItemSubrole_ :: IO CFStringRef
foreign import ccall unsafe kAXURLDockItemSubrole_ :: IO CFStringRef
foreign import ccall unsafe kAXDockExtraDockItemSubrole_ :: IO CFStringRef
foreign import ccall unsafe kAXTrashDockItemSubrole_ :: IO CFStringRef
foreign import ccall unsafe kAXProcessSwitcherListSubrole_ :: IO CFStringRef

toSubroleString :: Subrole -> IO CFStringRef
toSubroleString = \case
  CloseButtonSubrole -> kAXCloseButtonSubrole_
  MinimizeButtonSubrole -> kAXMinimizeButtonSubrole_
  ZoomButtonSubrole -> kAXZoomButtonSubrole_
  ToolbarButtonSubrole -> kAXToolbarButtonSubrole_
  SecureTextFieldSubrole -> kAXSecureTextFieldSubrole_
  TableRowSubrole -> kAXTableRowSubrole_
  OutlineRowSubrole -> kAXOutlineRowSubrole_
  UnknownSubrole -> kAXUnknownSubrole_
  StandardWindowSubrole -> kAXStandardWindowSubrole_
  DialogSubrole -> kAXDialogSubrole_
  SystemDialogSubrole -> kAXSystemDialogSubrole_
  FloatingWindowSubrole -> kAXFloatingWindowSubrole_
  SystemFloatingWindowSubrole -> kAXSystemFloatingWindowSubrole_
  IncrementArrowSubrole -> kAXIncrementArrowSubrole_
  DecrementArrowSubrole -> kAXDecrementArrowSubrole_
  IncrementPageSubrole -> kAXIncrementPageSubrole_
  DecrementPageSubrole -> kAXDecrementPageSubrole_
  SortButtonSubrole -> kAXSortButtonSubrole_
  SearchFieldSubrole -> kAXSearchFieldSubrole_
  ApplicationDockItemSubrole -> kAXApplicationDockItemSubrole_
  DocumentDockItemSubrole -> kAXDocumentDockItemSubrole_
  FolderDockItemSubrole -> kAXFolderDockItemSubrole_
  MinimizedWindowDockItemSubrole -> kAXMinimizedWindowDockItemSubrole_
  URLDockItemSubrole -> kAXURLDockItemSubrole_
  DockExtraDockItemSubrole -> kAXDockExtraDockItemSubrole_
  TrashDockItemSubrole -> kAXTrashDockItemSubrole_
  ProcessSwitcherListSubrole -> kAXProcessSwitcherListSubrole_
  SubroleOther str ->
    fromString CFStringEncodingASCII str >>= flip withCFPtr pure

fromSubroleCFString :: CFString -> IO Subrole
fromSubroleCFString subrole = do
  Just sr <- toString CFStringEncodingASCII subrole
  withCFPtr subrole $ \subr ->
    fmap (fromMaybe (SubroleOther sr)) . runMaybeT . msum . fmap MaybeT $
    [ kAXCloseButtonSubrole_ >>= \str -> cfEqual str subr >>= \b ->
        pure (if b then Just CloseButtonSubrole else Nothing)
    , kAXMinimizeButtonSubrole_ >>= \str -> cfEqual str subr >>= \b ->
        pure (if b then Just MinimizeButtonSubrole else Nothing)
    , kAXZoomButtonSubrole_ >>= \str -> cfEqual str subr >>= \b ->
        pure (if b then Just ZoomButtonSubrole else Nothing)
    , kAXToolbarButtonSubrole_ >>= \str -> cfEqual str subr >>= \b ->
        pure (if b then Just ToolbarButtonSubrole else Nothing)
    , kAXSecureTextFieldSubrole_ >>= \str -> cfEqual str subr >>= \b ->
        pure (if b then Just SecureTextFieldSubrole else Nothing)
    , kAXTableRowSubrole_ >>= \str -> cfEqual str subr >>= \b ->
        pure (if b then Just TableRowSubrole else Nothing)
    , kAXOutlineRowSubrole_ >>= \str -> cfEqual str subr >>= \b ->
        pure (if b then Just OutlineRowSubrole else Nothing)
    , kAXUnknownSubrole_ >>= \str -> cfEqual str subr >>= \b ->
        pure (if b then Just UnknownSubrole else Nothing)
    , kAXStandardWindowSubrole_ >>= \str -> cfEqual str subr >>= \b ->
        pure (if b then Just StandardWindowSubrole else Nothing)
    , kAXDialogSubrole_ >>= \str -> cfEqual str subr >>= \b ->
        pure (if b then Just DialogSubrole else Nothing)
    , kAXSystemDialogSubrole_ >>= \str -> cfEqual str subr >>= \b ->
        pure (if b then Just SystemDialogSubrole else Nothing)
    , kAXFloatingWindowSubrole_ >>= \str -> cfEqual str subr >>= \b ->
        pure (if b then Just FloatingWindowSubrole else Nothing)
    , kAXSystemFloatingWindowSubrole_ >>= \str -> cfEqual str subr >>= \b ->
        pure (if b then Just SystemFloatingWindowSubrole else Nothing)
    , kAXIncrementArrowSubrole_ >>= \str -> cfEqual str subr >>= \b ->
        pure (if b then Just IncrementArrowSubrole else Nothing)
    , kAXDecrementArrowSubrole_ >>= \str -> cfEqual str subr >>= \b ->
        pure (if b then Just DecrementArrowSubrole else Nothing)
    , kAXIncrementPageSubrole_ >>= \str -> cfEqual str subr >>= \b ->
        pure (if b then Just IncrementPageSubrole else Nothing)
    , kAXDecrementPageSubrole_ >>= \str -> cfEqual str subr >>= \b ->
        pure (if b then Just DecrementPageSubrole else Nothing)
    , kAXSortButtonSubrole_ >>= \str -> cfEqual str subr >>= \b ->
        pure (if b then Just SortButtonSubrole else Nothing)
    , kAXSearchFieldSubrole_ >>= \str -> cfEqual str subr >>= \b ->
        pure (if b then Just SearchFieldSubrole else Nothing)
    , kAXApplicationDockItemSubrole_ >>= \str -> cfEqual str subr >>= \b ->
        pure (if b then Just ApplicationDockItemSubrole else Nothing)
    , kAXDocumentDockItemSubrole_ >>= \str -> cfEqual str subr >>= \b ->
        pure (if b then Just DocumentDockItemSubrole else Nothing)
    , kAXFolderDockItemSubrole_ >>= \str -> cfEqual str subr >>= \b ->
        pure (if b then Just FolderDockItemSubrole else Nothing)
    , kAXMinimizedWindowDockItemSubrole_ >>= \str -> cfEqual str subr >>= \b ->
        pure (if b then Just MinimizedWindowDockItemSubrole else Nothing)
    , kAXURLDockItemSubrole_ >>= \str -> cfEqual str subr >>= \b ->
        pure (if b then Just URLDockItemSubrole else Nothing)
    , kAXDockExtraDockItemSubrole_ >>= \str -> cfEqual str subr >>= \b ->
        pure (if b then Just DockExtraDockItemSubrole else Nothing)
    , kAXTrashDockItemSubrole_ >>= \str -> cfEqual str subr >>= \b ->
        pure (if b then Just TrashDockItemSubrole else Nothing)
    , kAXProcessSwitcherListSubrole_ >>= \str -> cfEqual str subr >>= \b ->
        pure (if b then Just ProcessSwitcherListSubrole else Nothing)
    ]
