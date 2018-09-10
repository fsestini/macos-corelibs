{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE KindSignatures #-}

module MacSdk.Framework.Accessibility.Attribute.Types where

import MacSdk.Framework.CoreFoundation
import MacSdk.Framework.Accessibility.Role
import MacSdk.Framework.Accessibility.UIElement
import MacSdk.Framework.CoreGraphics.Rect

import Foreign (castPtr)
import Control.Monad.Trans.Maybe
import Control.Monad (msum, join)
import Data.Maybe (fromMaybe)
import Data.Bifunctor (bimap)

data Attribute :: * -> * where
  -- | The role, or type, of this accessibility object (for example, AXButton).
  -- This string is for identification purposes only and does not need to be
  -- localized. All accessibility objects must include this attribute.
  RoleAttribute :: Attribute Role
  SubroleAttribute :: Attribute Subrole
  RoleDescriptionAttribute :: Attribute CFObject
  -- | A localized string containing help text for this accessibility object. An
  -- accessibility object that provides help information should include this
  -- attribute.
  HelpAttribute :: Attribute CFObject
  TitleAttribute :: Attribute CFString
  ValueAttribute :: Attribute CFObject
  -- | The minimum value this accessibility object can display (for example, the
  -- minimum value of a scroller control). This attribute is used only in
  -- conjunction with the AXValue attribute.
  MinValueAttribute :: Attribute CFObject
  -- | The maximum value this accessibility object can display (for example, the
  -- maximum value of a scroller control). This attribute is used only in
  -- conjunction with the AXValue attribute.
  MaxValueAttribute :: Attribute CFObject
  -- | The amount an accessibility object’s value changes as the result of a
  -- single action (for example, how far a scroller travels with one mouse
  -- click). This attribute is used only in conjunction with the AXValue
  -- attribute.
  ValueIncrementAttribute :: Attribute CFObject
  AllowedValuesAttribute :: Attribute CFObject
  EnabledAttribute :: Attribute CFObject
  FocusedAttribute :: Attribute Bool
  -- | This accessibility object’s parent object in the accessibility hierarchy.
  -- This attribute is required for all accessibility objects except the
  -- application-level accessibility object.
  ParentAttribute :: Attribute UIElement
  ChildrenAttribute :: Attribute CFObject
  SelectedChildrenAttribute :: Attribute CFObject
  VisibleChildrenAttribute :: Attribute CFObject
  WindowAttribute :: Attribute CFObject
  PositionAttribute :: Attribute Point
  TopLevelUIElementAttribute :: Attribute UIElement
  -- | The vertical and horizontal dimensions of this accessibility object. This
  -- attribute is required for all accessibility objects that are visible on the
  -- screen.
  SizeAttribute :: Attribute Size
  OrientationAttribute :: Attribute CFObject
  DescriptionAttribute :: Attribute CFObject
  -- | The currently selected text within this accessibility object. This
  -- attribute is required for all accessibility objects that represent editable
  -- text elements.
  SelectedTextAttribute :: Attribute CFObject
  -- | Indicates the range of characters (not bytes) that defines the currently
  -- selected text within this accessibility object. This attribute is required
  -- for all accessibility objects that represent editable text elements.
  SelectedTextRangeAttribute :: Attribute CFObject
  VisibleCharacterRangeAttribute :: Attribute CFObject
  -- | The total number of characters (not bytes) in the editable text element
  -- represented by this accessibility object. This attribute is required for
  -- all accessibility objects that represent editable text elements.
  NumberOfCharactersAttribute :: Attribute CFObject
  SharedTextUIElementsAttribute :: Attribute CFObject
  SharedCharacterRangeAttribute :: Attribute CFObject
  MainAttribute :: Attribute Bool
  -- | Indicates whether the window represented by this accessibility object is
  -- currently minimized in the Dock. This attribute is recommended for all
  -- accessibility objects that represent windows that can be minimized.
  MinimizedAttribute :: Attribute Bool
  CloseButtonAttribute :: Attribute UIElement
  ZoomButtonAttribute :: Attribute UIElement
  MinimizeButtonAttribute :: Attribute UIElement
  ToolbarButtonAttribute :: Attribute UIElement
  GrowAreaAttribute :: Attribute CFObject
  ProxyAttribute :: Attribute CFObject
  -- | Indicates whether the window represented by this accessibility object is
  -- modal. This attribute is recommended for all accessibility objects that
  -- represent windows.
  ModalAttribute :: Attribute CFObject
  DefaultButtonAttribute :: Attribute CFObject
  CancelButtonAttribute :: Attribute CFObject
  -- | The primary key in the keyboard shortcut for the command represented by
  -- this accessibility object. For example, “O” is the primary key in the
  -- keyboard shortcut for the Open command.
  MenuItemCmdCharAttribute :: Attribute CFObject
  MenuItemCmdVirtualKeyAttribute :: Attribute CFObject
  MenuItemCmdGlyphAttribute :: Attribute CFObject
  -- | An integer mask that represents the modifier keys held down in the
  -- keyboard shortcut for the command represented by this accessibility object.
  MenuItemCmdModifiersAttribute :: Attribute CFObject
  MenuItemMarkCharAttribute :: Attribute CFObject
  MenuItemPrimaryUIElementAttribute :: Attribute CFObject
  MenuBarAttribute :: Attribute CFObject
  -- | An array of accessibility objects representing this application’s
  -- windows. This attribute is recommended for all application-level
  -- accessibility objects.
  WindowsAttribute :: Attribute (Array UIElement)
  -- | Indicates whether the application represented by this accessibility
  -- object is active. This attribute is recommended for all application-level
  -- accessibility objects.
  FrontmostAttribute :: Attribute CFObject
  -- | Indicates whether the application represented by this accessibility
  -- object is hidden. This attribute is recommended for all application-level
  -- accessibility objects.
  HiddenAttribute :: Attribute CFObject
  -- | The accessibility object representing this application’s main window.
  -- This attribute is recommended for all application-level accessibility
  -- objects.
  MainWindowAttribute :: Attribute CFObject
  -- | The accessibility object that represents the currently focused window of
  -- this application. This attribute is recommended for all application-level
  -- accessibility objects.
  -- FocusedUIElemenAttribute :: Attribute FocusedUIElemenAttributeTag
  -- The accessibility object that represents the currently focused user
  -- interface element in this application. This attribute is recommended for
  -- all application-level accessibility objects.
  FocusedWindowAttribute :: Attribute UIElement
  HeaderAttribute :: Attribute CFObject
  EditedAttribute :: Attribute CFObject
  -- | An accessibility object that represents a static text title associated
  -- with another accessibility object.
  TitleUIElementAttribute :: Attribute CFObject
  -- | Indicates whether the value displayed in the user interface element
  -- represented by this accessibility object wraps around.
  ValueWrapsAttribute :: Attribute CFObject
  TabsAttribute :: Attribute CFObject
  HorizontalScrollBarAttribute :: Attribute CFObject
  VerticalScrollBarAttribute :: Attribute CFObject
  -- | Identifies which child of an accessibility object representing a toolbar
  -- is the overflow button (if any). This attribute is optional.
  OverflowButtonAttribute :: Attribute CFObject
  -- | The filename associated with this accessibility object. This attribute is
  -- optional.
  FilenameAttribute :: Attribute CFObject
  -- | Indicates whether the menu displayed by the combo box or pop-up menu
  -- represented by this accessibility object is currently expanded. This
  -- attribute is recommended for all accessibility objects that display a
  -- pop-up menu.
  ExpandedAttribute :: Attribute CFObject
  -- | Indicates whether the row or column element represented by this
  -- accessibility object is selected. This attribute is recommended for all
  -- accessibility objects that represent selectable rows or columns.
  SelectedAttribute :: Attribute CFObject
  -- | An array of views and splitter bar elements displayed by the split view
  -- represented by this accessibility object. This is a convenience attribute
  -- that helps an assistive application easily find these elements.
  SplittersAttribute :: Attribute CFObject
  NextContentsAttribute :: Attribute CFObject
  PreviousContentsAttribute :: Attribute CFObject
  -- | The URL of the open document represented by this accessibility object.
  -- This attribute represents the URL as a string object.
  DocumentAttribute :: Attribute CFObject
  IncrementButtonAttribute :: Attribute CFObject
  -- | The decrement element associated with the user interface object this
  -- accessibility object represents. This attribute can be used to provide
  -- convenient access to the decrement area of a custom user interface object.
  DecrementButtonAttribute :: Attribute CFObject
  ContentsAttribute :: Attribute CFObject
  -- | The incrementor of a time or date field represented by this accessibility
  -- object. This attribute is required for accessibility objects that represent
  -- time or date field elements that display an incrementor.
  IncrementorAttribute :: Attribute CFObject
  -- | The hour field of a time field represented by this accessibility object.
  -- This attribute is required for accessibility objects that represent time
  -- fields that display hours.
  HourFieldAttribute :: Attribute CFObject
  -- | The minute field of a time field represented by this accessibility
  -- object. This attribute is required for accessibility objects that represent
  -- time fields that display minutes.
  MinuteFieldAttribute :: Attribute CFObject
  -- | The second field of a time field represented by this accessibility
  -- object. This attribute is required for accessibility objects that represent
  -- time fields that display seconds.
  SecondFieldAttribute :: Attribute CFObject
  -- | The AM/PM field of a time field represented by this accessibility object.
  -- This attribute is required for accessibility objects that represent time
  -- fields that display AM/PM settings.
  AMPMFieldAttribute :: Attribute CFObject
  -- | The day field of a time field represented by this accessibility object.
  -- This attribute is required for accessibility objects that represent time
  -- fields that display days.
  DayFieldAttribute :: Attribute CFObject
  -- | The month field of a time field represented by this accessibility object.
  -- This attribute is required for accessibility objects that represent time
  -- fields that display months.
  MonthFieldAttribute :: Attribute CFObject
  -- | The year field of a time field represented by this accessibility object.
  -- This attribute is required for accessibility objects that represent time
  -- fields that display years.
  YearFieldAttribute :: Attribute CFObject
  ColumnTitleAttribute :: Attribute CFObject
  -- | The URL that describes the location of the document or application
  -- represented by this accessibility object.
  URLAttribute :: Attribute CFObject
  LabelUIElementsAttribute :: Attribute CFObject
  -- | The value of the label represented by this accessibility object. This
  -- attribute is required for all accessibility objects that represent labels.
  LabelValueAttribute :: Attribute CFObject
  -- | An array of accessibility objects that represent the contextual or Dock
  -- menus provided by this accessibility object.
  ShownMenuUIElementAttribute :: Attribute CFObject
  ServesAsTitleForUIElementsAttribute :: Attribute CFObject
  LinkedUIElementsAttribute :: Attribute CFObject
  -- | An array of the accessibility objects representing the rows in this table
  -- or outline view.
  RowsAttribute :: Attribute CFObject
  -- | An array of the accessibility objects representing the currently visible
  -- rows in this table or outline view.
  VisibleRowsAttribute :: Attribute CFObject
  -- | An array of the accessibility objects representing the currently selected
  -- rows in this table or outline view.
  SelectedRowsAttribute :: Attribute CFObject
  -- | An array of the accessibility objects representing the columns in this
  -- browser view.
  ColumnsAttribute :: Attribute CFObject
  -- | An array of the accessibility objects representing the currently visible
  -- columns in this browser view.
  VisibleColumnsAttribute :: Attribute CFObject
  -- | An array of the accessibility objects representing the currently selected
  -- columns in this browser view.
  SelectedColumnsAttribute :: Attribute CFObject
  -- | The sort direction of this accessibility object’s contents. For example,
  -- a list view’s contents may be sorted in ascending or descending order.
  SortDirectionAttribute :: Attribute CFObject
  -- | An array of accessibility objects representing the column headers of this
  -- table or browser view.
  ColumnHeaderUIElementsAttribute :: Attribute CFObject
  -- | The index of the row or column represented by this accessibility object.
  IndexAttribute :: Attribute CFObject
  -- | Indicates whether a row in an outline view represented by this
  -- accessibility object has an open or closed disclosure triangle. true
  -- indicates an open disclosure triangle; false indicates a closed disclosure
  -- triangle.
  DisclosingAttribute :: Attribute CFObject
  -- | An array of accessibility objects representing the disclosed rows of this
  -- user interface element.
  DisclosedRowsAttribute :: Attribute CFObject
  -- | The accessibility object representing the disclosing row.
  DisclosedByRowAttribute :: Attribute CFObject
  -- | The accessibility object that represents the area available to the user
  -- through the matte hole.
  MatteHoleAttribute :: Attribute CFObject
  -- | The accessibility object clipped by the matte.
  MatteContentUIElementAttribute :: Attribute CFObject
  -- | Indicates if the application represented by the Dock icon this
  -- accessibility object represents is currently running.
  IsApplicationRunningAttribute :: Attribute CFObject
  FocusedApplicationAttribute :: Attribute CFObject
  -- | The line number of the insertion point in the text associated with this
  -- accessibility object.
  InsertionPointLineNumberAttribute :: Attribute CFObject

deriving instance Show (Attribute tag)

foreign import ccall unsafe kAXRoleAttribute_ :: IO CFStringRef
foreign import ccall unsafe kAXSubroleAttribute_ :: IO CFStringRef
foreign import ccall unsafe kAXRoleDescriptionAttribute_ :: IO CFStringRef
foreign import ccall unsafe kAXHelpAttribute_ :: IO CFStringRef
foreign import ccall unsafe kAXTitleAttribute_ :: IO CFStringRef
foreign import ccall unsafe kAXValueAttribute_ :: IO CFStringRef
foreign import ccall unsafe kAXMinValueAttribute_ :: IO CFStringRef
foreign import ccall unsafe kAXMaxValueAttribute_ :: IO CFStringRef
foreign import ccall unsafe kAXValueIncrementAttribute_ :: IO CFStringRef
foreign import ccall unsafe kAXAllowedValuesAttribute_ :: IO CFStringRef
foreign import ccall unsafe kAXEnabledAttribute_ :: IO CFStringRef
foreign import ccall unsafe kAXFocusedAttribute_ :: IO CFStringRef
foreign import ccall unsafe kAXParentAttribute_ :: IO CFStringRef
foreign import ccall unsafe kAXChildrenAttribute_ :: IO CFStringRef
foreign import ccall unsafe kAXSelectedChildrenAttribute_ :: IO CFStringRef
foreign import ccall unsafe kAXVisibleChildrenAttribute_ :: IO CFStringRef
foreign import ccall unsafe kAXWindowAttribute_ :: IO CFStringRef
foreign import ccall unsafe kAXPositionAttribute_ :: IO CFStringRef
foreign import ccall unsafe kAXTopLevelUIElementAttribute_ :: IO CFStringRef
foreign import ccall unsafe kAXSizeAttribute_ :: IO CFStringRef
foreign import ccall unsafe kAXOrientationAttribute_ :: IO CFStringRef
foreign import ccall unsafe kAXDescriptionAttribute_ :: IO CFStringRef
foreign import ccall unsafe kAXSelectedTextAttribute_ :: IO CFStringRef
foreign import ccall unsafe kAXSelectedTextRangeAttribute_ :: IO CFStringRef
foreign import ccall unsafe kAXVisibleCharacterRangeAttribute_ :: IO CFStringRef
foreign import ccall unsafe kAXNumberOfCharactersAttribute_ :: IO CFStringRef
foreign import ccall unsafe kAXSharedTextUIElementsAttribute_ :: IO CFStringRef
foreign import ccall unsafe kAXSharedCharacterRangeAttribute_ :: IO CFStringRef
foreign import ccall unsafe kAXMainAttribute_ :: IO CFStringRef
foreign import ccall unsafe kAXMinimizedAttribute_ :: IO CFStringRef
foreign import ccall unsafe kAXCloseButtonAttribute_ :: IO CFStringRef
foreign import ccall unsafe kAXZoomButtonAttribute_ :: IO CFStringRef
foreign import ccall unsafe kAXMinimizeButtonAttribute_ :: IO CFStringRef
foreign import ccall unsafe kAXToolbarButtonAttribute_ :: IO CFStringRef
foreign import ccall unsafe kAXGrowAreaAttribute_ :: IO CFStringRef
foreign import ccall unsafe kAXProxyAttribute_ :: IO CFStringRef
foreign import ccall unsafe kAXModalAttribute_ :: IO CFStringRef
foreign import ccall unsafe kAXDefaultButtonAttribute_ :: IO CFStringRef
foreign import ccall unsafe kAXCancelButtonAttribute_ :: IO CFStringRef
foreign import ccall unsafe kAXMenuItemCmdCharAttribute_ :: IO CFStringRef
foreign import ccall unsafe kAXMenuItemCmdVirtualKeyAttribute_ :: IO CFStringRef
foreign import ccall unsafe kAXMenuItemCmdGlyphAttribute_ :: IO CFStringRef
foreign import ccall unsafe kAXMenuItemCmdModifiersAttribute_ :: IO CFStringRef
foreign import ccall unsafe kAXMenuItemMarkCharAttribute_ :: IO CFStringRef
foreign import ccall unsafe kAXMenuItemPrimaryUIElementAttribute_ :: IO CFStringRef
foreign import ccall unsafe kAXMenuBarAttribute_ :: IO CFStringRef
foreign import ccall unsafe kAXWindowsAttribute_ :: IO CFStringRef
foreign import ccall unsafe kAXFrontmostAttribute_ :: IO CFStringRef
foreign import ccall unsafe kAXHiddenAttribute_ :: IO CFStringRef
foreign import ccall unsafe kAXMainWindowAttribute_ :: IO CFStringRef
foreign import ccall unsafe kAXFocusedWindowAttribute_ :: IO CFStringRef
-- foreign import ccall unsafe kAXFocusedUIElemenAttribute_ :: IO CFStringRef
foreign import ccall unsafe kAXHeaderAttribute_ :: IO CFStringRef
foreign import ccall unsafe kAXEditedAttribute_ :: IO CFStringRef
foreign import ccall unsafe kAXTitleUIElementAttribute_ :: IO CFStringRef
foreign import ccall unsafe kAXValueWrapsAttribute_ :: IO CFStringRef
foreign import ccall unsafe kAXTabsAttribute_ :: IO CFStringRef
foreign import ccall unsafe kAXHorizontalScrollBarAttribute_ :: IO CFStringRef
foreign import ccall unsafe kAXVerticalScrollBarAttribute_ :: IO CFStringRef
foreign import ccall unsafe kAXOverflowButtonAttribute_ :: IO CFStringRef
foreign import ccall unsafe kAXFilenameAttribute_ :: IO CFStringRef
foreign import ccall unsafe kAXExpandedAttribute_ :: IO CFStringRef
foreign import ccall unsafe kAXSelectedAttribute_ :: IO CFStringRef
foreign import ccall unsafe kAXSplittersAttribute_ :: IO CFStringRef
foreign import ccall unsafe kAXNextContentsAttribute_ :: IO CFStringRef
foreign import ccall unsafe kAXPreviousContentsAttribute_ :: IO CFStringRef
foreign import ccall unsafe kAXDocumentAttribute_ :: IO CFStringRef
foreign import ccall unsafe kAXIncrementButtonAttribute_ :: IO CFStringRef
foreign import ccall unsafe kAXDecrementButtonAttribute_ :: IO CFStringRef
foreign import ccall unsafe kAXContentsAttribute_ :: IO CFStringRef
foreign import ccall unsafe kAXIncrementorAttribute_ :: IO CFStringRef
foreign import ccall unsafe kAXHourFieldAttribute_ :: IO CFStringRef
foreign import ccall unsafe kAXMinuteFieldAttribute_ :: IO CFStringRef
foreign import ccall unsafe kAXSecondFieldAttribute_ :: IO CFStringRef
foreign import ccall unsafe kAXAMPMFieldAttribute_ :: IO CFStringRef
foreign import ccall unsafe kAXDayFieldAttribute_ :: IO CFStringRef
foreign import ccall unsafe kAXMonthFieldAttribute_ :: IO CFStringRef
foreign import ccall unsafe kAXYearFieldAttribute_ :: IO CFStringRef
foreign import ccall unsafe kAXColumnTitleAttribute_ :: IO CFStringRef
foreign import ccall unsafe kAXURLAttribute_ :: IO CFStringRef
foreign import ccall unsafe kAXLabelUIElementsAttribute_ :: IO CFStringRef
foreign import ccall unsafe kAXLabelValueAttribute_ :: IO CFStringRef
foreign import ccall unsafe kAXShownMenuUIElementAttribute_ :: IO CFStringRef
foreign import ccall unsafe kAXServesAsTitleForUIElementsAttribute_ :: IO CFStringRef
foreign import ccall unsafe kAXLinkedUIElementsAttribute_ :: IO CFStringRef
foreign import ccall unsafe kAXRowsAttribute_ :: IO CFStringRef
foreign import ccall unsafe kAXVisibleRowsAttribute_ :: IO CFStringRef
foreign import ccall unsafe kAXSelectedRowsAttribute_ :: IO CFStringRef
foreign import ccall unsafe kAXColumnsAttribute_ :: IO CFStringRef
foreign import ccall unsafe kAXVisibleColumnsAttribute_ :: IO CFStringRef
foreign import ccall unsafe kAXSelectedColumnsAttribute_ :: IO CFStringRef
foreign import ccall unsafe kAXSortDirectionAttribute_ :: IO CFStringRef
foreign import ccall unsafe kAXColumnHeaderUIElementsAttribute_ :: IO CFStringRef
foreign import ccall unsafe kAXIndexAttribute_ :: IO CFStringRef
foreign import ccall unsafe kAXDisclosingAttribute_ :: IO CFStringRef
foreign import ccall unsafe kAXDisclosedRowsAttribute_ :: IO CFStringRef
foreign import ccall unsafe kAXDisclosedByRowAttribute_ :: IO CFStringRef
foreign import ccall unsafe kAXMatteHoleAttribute_ :: IO CFStringRef
foreign import ccall unsafe kAXMatteContentUIElementAttribute_ :: IO CFStringRef
foreign import ccall unsafe kAXIsApplicationRunningAttribute_ :: IO CFStringRef
foreign import ccall unsafe kAXFocusedApplicationAttribute_ :: IO CFStringRef
foreign import ccall unsafe kAXInsertionPointLineNumberAttribute_ :: IO CFStringRef

toAttributeString :: Attribute a -> IO String
toAttributeString a =
  toAttributeString' a >>= manageCFObj >>=
    fmap (fromMaybe (error "toAttributeString: decoding failed")) .
      toString CFStringEncodingASCII

toAttributeString' :: Attribute a -> IO CFStringRef
toAttributeString' = \case
  RoleAttribute -> kAXRoleAttribute_
  SubroleAttribute -> kAXSubroleAttribute_
  RoleDescriptionAttribute -> kAXRoleDescriptionAttribute_
  HelpAttribute -> kAXHelpAttribute_
  TitleAttribute -> kAXTitleAttribute_
  ValueAttribute -> kAXValueAttribute_
  MinValueAttribute -> kAXMinValueAttribute_
  MaxValueAttribute -> kAXMaxValueAttribute_
  ValueIncrementAttribute -> kAXValueIncrementAttribute_
  AllowedValuesAttribute -> kAXAllowedValuesAttribute_
  EnabledAttribute -> kAXEnabledAttribute_
  FocusedAttribute -> kAXFocusedAttribute_
  ParentAttribute -> kAXParentAttribute_
  ChildrenAttribute -> kAXChildrenAttribute_
  SelectedChildrenAttribute -> kAXSelectedChildrenAttribute_
  VisibleChildrenAttribute -> kAXVisibleChildrenAttribute_
  WindowAttribute -> kAXWindowAttribute_
  PositionAttribute -> kAXPositionAttribute_
  TopLevelUIElementAttribute -> kAXTopLevelUIElementAttribute_
  SizeAttribute -> kAXSizeAttribute_
  OrientationAttribute -> kAXOrientationAttribute_
  DescriptionAttribute -> kAXDescriptionAttribute_
  SelectedTextAttribute -> kAXSelectedTextAttribute_
  SelectedTextRangeAttribute -> kAXSelectedTextRangeAttribute_
  VisibleCharacterRangeAttribute -> kAXVisibleCharacterRangeAttribute_
  NumberOfCharactersAttribute -> kAXNumberOfCharactersAttribute_
  SharedTextUIElementsAttribute -> kAXSharedTextUIElementsAttribute_
  SharedCharacterRangeAttribute -> kAXSharedCharacterRangeAttribute_
  MainAttribute -> kAXMainAttribute_
  MinimizedAttribute -> kAXMinimizedAttribute_
  CloseButtonAttribute -> kAXCloseButtonAttribute_
  ZoomButtonAttribute -> kAXZoomButtonAttribute_
  MinimizeButtonAttribute -> kAXMinimizeButtonAttribute_
  ToolbarButtonAttribute -> kAXToolbarButtonAttribute_
  GrowAreaAttribute -> kAXGrowAreaAttribute_
  ProxyAttribute -> kAXProxyAttribute_
  ModalAttribute -> kAXModalAttribute_
  DefaultButtonAttribute -> kAXDefaultButtonAttribute_
  CancelButtonAttribute -> kAXCancelButtonAttribute_
  MenuItemCmdCharAttribute -> kAXMenuItemCmdCharAttribute_
  MenuItemCmdVirtualKeyAttribute -> kAXMenuItemCmdVirtualKeyAttribute_
  MenuItemCmdGlyphAttribute -> kAXMenuItemCmdGlyphAttribute_
  MenuItemCmdModifiersAttribute -> kAXMenuItemCmdModifiersAttribute_
  MenuItemMarkCharAttribute -> kAXMenuItemMarkCharAttribute_
  MenuItemPrimaryUIElementAttribute -> kAXMenuItemPrimaryUIElementAttribute_
  MenuBarAttribute -> kAXMenuBarAttribute_
  WindowsAttribute -> kAXWindowsAttribute_
  FrontmostAttribute -> kAXFrontmostAttribute_
  HiddenAttribute -> kAXHiddenAttribute_
  MainWindowAttribute -> kAXMainWindowAttribute_
  FocusedWindowAttribute -> kAXFocusedWindowAttribute_
  -- FocusedUIElemenAttribute -> kAXFocusedUIElemenAttribute_
  HeaderAttribute -> kAXHeaderAttribute_
  EditedAttribute -> kAXEditedAttribute_
  TitleUIElementAttribute -> kAXTitleUIElementAttribute_
  ValueWrapsAttribute -> kAXValueWrapsAttribute_
  TabsAttribute -> kAXTabsAttribute_
  HorizontalScrollBarAttribute -> kAXHorizontalScrollBarAttribute_
  VerticalScrollBarAttribute -> kAXVerticalScrollBarAttribute_
  OverflowButtonAttribute -> kAXOverflowButtonAttribute_
  FilenameAttribute -> kAXFilenameAttribute_
  ExpandedAttribute -> kAXExpandedAttribute_
  SelectedAttribute -> kAXSelectedAttribute_
  SplittersAttribute -> kAXSplittersAttribute_
  NextContentsAttribute -> kAXNextContentsAttribute_
  PreviousContentsAttribute -> kAXPreviousContentsAttribute_
  DocumentAttribute -> kAXDocumentAttribute_
  IncrementButtonAttribute -> kAXIncrementButtonAttribute_
  DecrementButtonAttribute -> kAXDecrementButtonAttribute_
  ContentsAttribute -> kAXContentsAttribute_
  IncrementorAttribute -> kAXIncrementorAttribute_
  HourFieldAttribute -> kAXHourFieldAttribute_
  MinuteFieldAttribute -> kAXMinuteFieldAttribute_
  SecondFieldAttribute -> kAXSecondFieldAttribute_
  AMPMFieldAttribute -> kAXAMPMFieldAttribute_
  DayFieldAttribute -> kAXDayFieldAttribute_
  MonthFieldAttribute -> kAXMonthFieldAttribute_
  YearFieldAttribute -> kAXYearFieldAttribute_
  ColumnTitleAttribute -> kAXColumnTitleAttribute_
  URLAttribute -> kAXURLAttribute_
  LabelUIElementsAttribute -> kAXLabelUIElementsAttribute_
  LabelValueAttribute -> kAXLabelValueAttribute_
  ShownMenuUIElementAttribute -> kAXShownMenuUIElementAttribute_
  ServesAsTitleForUIElementsAttribute -> kAXServesAsTitleForUIElementsAttribute_
  LinkedUIElementsAttribute -> kAXLinkedUIElementsAttribute_
  RowsAttribute -> kAXRowsAttribute_
  VisibleRowsAttribute -> kAXVisibleRowsAttribute_
  SelectedRowsAttribute -> kAXSelectedRowsAttribute_
  ColumnsAttribute -> kAXColumnsAttribute_
  VisibleColumnsAttribute -> kAXVisibleColumnsAttribute_
  SelectedColumnsAttribute -> kAXSelectedColumnsAttribute_
  SortDirectionAttribute -> kAXSortDirectionAttribute_
  ColumnHeaderUIElementsAttribute -> kAXColumnHeaderUIElementsAttribute_
  IndexAttribute -> kAXIndexAttribute_
  DisclosingAttribute -> kAXDisclosingAttribute_
  DisclosedRowsAttribute -> kAXDisclosedRowsAttribute_
  DisclosedByRowAttribute -> kAXDisclosedByRowAttribute_
  MatteHoleAttribute -> kAXMatteHoleAttribute_
  MatteContentUIElementAttribute -> kAXMatteContentUIElementAttribute_
  IsApplicationRunningAttribute -> kAXIsApplicationRunningAttribute_
  FocusedApplicationAttribute -> kAXFocusedApplicationAttribute_
  InsertionPointLineNumberAttribute -> kAXInsertionPointLineNumberAttribute_

data SomeAttribute = forall tag . SomeAttribute (Attribute tag)

fromAttributeString :: CFStringRef -> IO (Maybe SomeAttribute)
fromAttributeString attr = runMaybeT . msum . fmap MaybeT $
  [ kAXRoleAttribute_ >>= \str -> cfEqual str attr >>= \b ->
      pure (if b then Just (SomeAttribute RoleAttribute) else Nothing)
  , kAXSubroleAttribute_ >>= \str -> cfEqual str attr >>= \b ->
      pure (if b then Just (SomeAttribute SubroleAttribute) else Nothing)
  , kAXRoleDescriptionAttribute_ >>= \str -> cfEqual str attr >>= \b ->
      pure (if b then Just (SomeAttribute RoleDescriptionAttribute) else Nothing)
  , kAXHelpAttribute_ >>= \str -> cfEqual str attr >>= \b ->
      pure (if b then Just (SomeAttribute HelpAttribute) else Nothing)
  , kAXTitleAttribute_ >>= \str -> cfEqual str attr >>= \b ->
      pure (if b then Just (SomeAttribute TitleAttribute) else Nothing)
  , kAXValueAttribute_ >>= \str -> cfEqual str attr >>= \b ->
      pure (if b then Just (SomeAttribute ValueAttribute) else Nothing)
  , kAXMinValueAttribute_ >>= \str -> cfEqual str attr >>= \b ->
      pure (if b then Just (SomeAttribute MinValueAttribute) else Nothing)
  , kAXMaxValueAttribute_ >>= \str -> cfEqual str attr >>= \b ->
      pure (if b then Just (SomeAttribute MaxValueAttribute) else Nothing)
  , kAXValueIncrementAttribute_ >>= \str -> cfEqual str attr >>= \b ->
      pure (if b then Just (SomeAttribute ValueIncrementAttribute) else Nothing)
  , kAXAllowedValuesAttribute_ >>= \str -> cfEqual str attr >>= \b ->
      pure (if b then Just (SomeAttribute AllowedValuesAttribute) else Nothing)
  , kAXEnabledAttribute_ >>= \str -> cfEqual str attr >>= \b ->
      pure (if b then Just (SomeAttribute EnabledAttribute) else Nothing)
  , kAXFocusedAttribute_ >>= \str -> cfEqual str attr >>= \b ->
      pure (if b then Just (SomeAttribute FocusedAttribute) else Nothing)
  , kAXParentAttribute_ >>= \str -> cfEqual str attr >>= \b ->
      pure (if b then Just (SomeAttribute ParentAttribute) else Nothing)
  , kAXChildrenAttribute_ >>= \str -> cfEqual str attr >>= \b ->
      pure (if b then Just (SomeAttribute ChildrenAttribute) else Nothing)
  , kAXSelectedChildrenAttribute_ >>= \str -> cfEqual str attr >>= \b ->
      pure (if b then Just (SomeAttribute SelectedChildrenAttribute) else Nothing)
  , kAXVisibleChildrenAttribute_ >>= \str -> cfEqual str attr >>= \b ->
      pure (if b then Just (SomeAttribute VisibleChildrenAttribute) else Nothing)
  , kAXWindowAttribute_ >>= \str -> cfEqual str attr >>= \b ->
      pure (if b then Just (SomeAttribute WindowAttribute) else Nothing)
  , kAXPositionAttribute_ >>= \str -> cfEqual str attr >>= \b ->
      pure (if b then Just (SomeAttribute PositionAttribute) else Nothing)
  , kAXTopLevelUIElementAttribute_ >>= \str -> cfEqual str attr >>= \b ->
      pure (if b then Just (SomeAttribute TopLevelUIElementAttribute) else Nothing)
  , kAXSizeAttribute_ >>= \str -> cfEqual str attr >>= \b ->
      pure (if b then Just (SomeAttribute SizeAttribute) else Nothing)
  , kAXOrientationAttribute_ >>= \str -> cfEqual str attr >>= \b ->
      pure (if b then Just (SomeAttribute OrientationAttribute) else Nothing)
  , kAXDescriptionAttribute_ >>= \str -> cfEqual str attr >>= \b ->
      pure (if b then Just (SomeAttribute DescriptionAttribute) else Nothing)
  , kAXSelectedTextAttribute_ >>= \str -> cfEqual str attr >>= \b ->
      pure (if b then Just (SomeAttribute SelectedTextAttribute) else Nothing)
  , kAXSelectedTextRangeAttribute_ >>= \str -> cfEqual str attr >>= \b ->
      pure (if b then Just (SomeAttribute SelectedTextRangeAttribute) else Nothing)
  , kAXVisibleCharacterRangeAttribute_ >>= \str -> cfEqual str attr >>= \b ->
      pure (if b then Just (SomeAttribute VisibleCharacterRangeAttribute) else Nothing)
  , kAXNumberOfCharactersAttribute_ >>= \str -> cfEqual str attr >>= \b ->
      pure (if b then Just (SomeAttribute NumberOfCharactersAttribute) else Nothing)
  , kAXSharedTextUIElementsAttribute_ >>= \str -> cfEqual str attr >>= \b ->
      pure (if b then Just (SomeAttribute SharedTextUIElementsAttribute) else Nothing)
  , kAXSharedCharacterRangeAttribute_ >>= \str -> cfEqual str attr >>= \b ->
      pure (if b then Just (SomeAttribute SharedCharacterRangeAttribute) else Nothing)
  , kAXMainAttribute_ >>= \str -> cfEqual str attr >>= \b ->
      pure (if b then Just (SomeAttribute MainAttribute) else Nothing)
  , kAXMinimizedAttribute_ >>= \str -> cfEqual str attr >>= \b ->
      pure (if b then Just (SomeAttribute MinimizedAttribute) else Nothing)
  , kAXCloseButtonAttribute_ >>= \str -> cfEqual str attr >>= \b ->
      pure (if b then Just (SomeAttribute CloseButtonAttribute) else Nothing)
  , kAXZoomButtonAttribute_ >>= \str -> cfEqual str attr >>= \b ->
      pure (if b then Just (SomeAttribute ZoomButtonAttribute) else Nothing)
  , kAXMinimizeButtonAttribute_ >>= \str -> cfEqual str attr >>= \b ->
      pure (if b then Just (SomeAttribute MinimizeButtonAttribute) else Nothing)
  , kAXToolbarButtonAttribute_ >>= \str -> cfEqual str attr >>= \b ->
      pure (if b then Just (SomeAttribute ToolbarButtonAttribute) else Nothing)
  , kAXGrowAreaAttribute_ >>= \str -> cfEqual str attr >>= \b ->
      pure (if b then Just (SomeAttribute GrowAreaAttribute) else Nothing)
  , kAXProxyAttribute_ >>= \str -> cfEqual str attr >>= \b ->
      pure (if b then Just (SomeAttribute ProxyAttribute) else Nothing)
  , kAXModalAttribute_ >>= \str -> cfEqual str attr >>= \b ->
      pure (if b then Just (SomeAttribute ModalAttribute) else Nothing)
  , kAXDefaultButtonAttribute_ >>= \str -> cfEqual str attr >>= \b ->
      pure (if b then Just (SomeAttribute DefaultButtonAttribute) else Nothing)
  , kAXCancelButtonAttribute_ >>= \str -> cfEqual str attr >>= \b ->
      pure (if b then Just (SomeAttribute CancelButtonAttribute) else Nothing)
  , kAXMenuItemCmdCharAttribute_ >>= \str -> cfEqual str attr >>= \b ->
      pure (if b then Just (SomeAttribute MenuItemCmdCharAttribute) else Nothing)
  , kAXMenuItemCmdVirtualKeyAttribute_ >>= \str -> cfEqual str attr >>= \b ->
      pure (if b then Just (SomeAttribute MenuItemCmdVirtualKeyAttribute) else Nothing)
  , kAXMenuItemCmdGlyphAttribute_ >>= \str -> cfEqual str attr >>= \b ->
      pure (if b then Just (SomeAttribute MenuItemCmdGlyphAttribute) else Nothing)
  , kAXMenuItemCmdModifiersAttribute_ >>= \str -> cfEqual str attr >>= \b ->
      pure (if b then Just (SomeAttribute MenuItemCmdModifiersAttribute) else Nothing)
  , kAXMenuItemMarkCharAttribute_ >>= \str -> cfEqual str attr >>= \b ->
      pure (if b then Just (SomeAttribute MenuItemMarkCharAttribute) else Nothing)
  , kAXMenuItemPrimaryUIElementAttribute_ >>= \str -> cfEqual str attr >>= \b ->
      pure (if b then Just (SomeAttribute MenuItemPrimaryUIElementAttribute) else Nothing)
  , kAXMenuBarAttribute_ >>= \str -> cfEqual str attr >>= \b ->
      pure (if b then Just (SomeAttribute MenuBarAttribute) else Nothing)
  , kAXWindowsAttribute_ >>= \str -> cfEqual str attr >>= \b ->
      pure (if b then Just (SomeAttribute WindowsAttribute) else Nothing)
  , kAXFrontmostAttribute_ >>= \str -> cfEqual str attr >>= \b ->
      pure (if b then Just (SomeAttribute FrontmostAttribute) else Nothing)
  , kAXHiddenAttribute_ >>= \str -> cfEqual str attr >>= \b ->
      pure (if b then Just (SomeAttribute HiddenAttribute) else Nothing)
  , kAXMainWindowAttribute_ >>= \str -> cfEqual str attr >>= \b ->
      pure (if b then Just (SomeAttribute MainWindowAttribute) else Nothing)
  , kAXFocusedWindowAttribute_ >>= \str -> cfEqual str attr >>= \b ->
      pure (if b then Just (SomeAttribute FocusedWindowAttribute) else Nothing)
  -- , kAXFocusedUIElemenAttribute_ >>= \str -> cfEqual str attr >>= \b ->
  --     pure (if b then Just (SomeAttribute FocusedUIElemenAttribute) else Nothing)
  , kAXHeaderAttribute_ >>= \str -> cfEqual str attr >>= \b ->
      pure (if b then Just (SomeAttribute HeaderAttribute) else Nothing)
  , kAXEditedAttribute_ >>= \str -> cfEqual str attr >>= \b ->
      pure (if b then Just (SomeAttribute EditedAttribute) else Nothing)
  , kAXTitleUIElementAttribute_ >>= \str -> cfEqual str attr >>= \b ->
      pure (if b then Just (SomeAttribute TitleUIElementAttribute) else Nothing)
  , kAXValueWrapsAttribute_ >>= \str -> cfEqual str attr >>= \b ->
      pure (if b then Just (SomeAttribute ValueWrapsAttribute) else Nothing)
  , kAXTabsAttribute_ >>= \str -> cfEqual str attr >>= \b ->
      pure (if b then Just (SomeAttribute TabsAttribute) else Nothing)
  , kAXHorizontalScrollBarAttribute_ >>= \str -> cfEqual str attr >>= \b ->
      pure (if b then Just (SomeAttribute HorizontalScrollBarAttribute) else Nothing)
  , kAXVerticalScrollBarAttribute_ >>= \str -> cfEqual str attr >>= \b ->
      pure (if b then Just (SomeAttribute VerticalScrollBarAttribute) else Nothing)
  , kAXOverflowButtonAttribute_ >>= \str -> cfEqual str attr >>= \b ->
      pure (if b then Just (SomeAttribute OverflowButtonAttribute) else Nothing)
  , kAXFilenameAttribute_ >>= \str -> cfEqual str attr >>= \b ->
      pure (if b then Just (SomeAttribute FilenameAttribute) else Nothing)
  , kAXExpandedAttribute_ >>= \str -> cfEqual str attr >>= \b ->
      pure (if b then Just (SomeAttribute ExpandedAttribute) else Nothing)
  , kAXSelectedAttribute_ >>= \str -> cfEqual str attr >>= \b ->
      pure (if b then Just (SomeAttribute SelectedAttribute) else Nothing)
  , kAXSplittersAttribute_ >>= \str -> cfEqual str attr >>= \b ->
      pure (if b then Just (SomeAttribute SplittersAttribute) else Nothing)
  , kAXNextContentsAttribute_ >>= \str -> cfEqual str attr >>= \b ->
      pure (if b then Just (SomeAttribute NextContentsAttribute) else Nothing)
  , kAXPreviousContentsAttribute_ >>= \str -> cfEqual str attr >>= \b ->
      pure (if b then Just (SomeAttribute PreviousContentsAttribute) else Nothing)
  , kAXDocumentAttribute_ >>= \str -> cfEqual str attr >>= \b ->
      pure (if b then Just (SomeAttribute DocumentAttribute) else Nothing)
  , kAXIncrementButtonAttribute_ >>= \str -> cfEqual str attr >>= \b ->
      pure (if b then Just (SomeAttribute IncrementButtonAttribute) else Nothing)
  , kAXDecrementButtonAttribute_ >>= \str -> cfEqual str attr >>= \b ->
      pure (if b then Just (SomeAttribute DecrementButtonAttribute) else Nothing)
  , kAXContentsAttribute_ >>= \str -> cfEqual str attr >>= \b ->
      pure (if b then Just (SomeAttribute ContentsAttribute) else Nothing)
  , kAXIncrementorAttribute_ >>= \str -> cfEqual str attr >>= \b ->
      pure (if b then Just (SomeAttribute IncrementorAttribute) else Nothing)
  , kAXHourFieldAttribute_ >>= \str -> cfEqual str attr >>= \b ->
      pure (if b then Just (SomeAttribute HourFieldAttribute) else Nothing)
  , kAXMinuteFieldAttribute_ >>= \str -> cfEqual str attr >>= \b ->
      pure (if b then Just (SomeAttribute MinuteFieldAttribute) else Nothing)
  , kAXSecondFieldAttribute_ >>= \str -> cfEqual str attr >>= \b ->
      pure (if b then Just (SomeAttribute SecondFieldAttribute) else Nothing)
  , kAXAMPMFieldAttribute_ >>= \str -> cfEqual str attr >>= \b ->
      pure (if b then Just (SomeAttribute AMPMFieldAttribute) else Nothing)
  , kAXDayFieldAttribute_ >>= \str -> cfEqual str attr >>= \b ->
      pure (if b then Just (SomeAttribute DayFieldAttribute) else Nothing)
  , kAXMonthFieldAttribute_ >>= \str -> cfEqual str attr >>= \b ->
      pure (if b then Just (SomeAttribute MonthFieldAttribute) else Nothing)
  , kAXYearFieldAttribute_ >>= \str -> cfEqual str attr >>= \b ->
      pure (if b then Just (SomeAttribute YearFieldAttribute) else Nothing)
  , kAXColumnTitleAttribute_ >>= \str -> cfEqual str attr >>= \b ->
      pure (if b then Just (SomeAttribute ColumnTitleAttribute) else Nothing)
  , kAXURLAttribute_ >>= \str -> cfEqual str attr >>= \b ->
      pure (if b then Just (SomeAttribute URLAttribute) else Nothing)
  , kAXLabelUIElementsAttribute_ >>= \str -> cfEqual str attr >>= \b ->
      pure (if b then Just (SomeAttribute LabelUIElementsAttribute) else Nothing)
  , kAXLabelValueAttribute_ >>= \str -> cfEqual str attr >>= \b ->
      pure (if b then Just (SomeAttribute LabelValueAttribute) else Nothing)
  , kAXShownMenuUIElementAttribute_ >>= \str -> cfEqual str attr >>= \b ->
      pure (if b then Just (SomeAttribute ShownMenuUIElementAttribute) else Nothing)
  , kAXServesAsTitleForUIElementsAttribute_ >>= \str -> cfEqual str attr >>= \b ->
      pure (if b then Just (SomeAttribute ServesAsTitleForUIElementsAttribute) else Nothing)
  , kAXLinkedUIElementsAttribute_ >>= \str -> cfEqual str attr >>= \b ->
      pure (if b then Just (SomeAttribute LinkedUIElementsAttribute) else Nothing)
  , kAXRowsAttribute_ >>= \str -> cfEqual str attr >>= \b ->
      pure (if b then Just (SomeAttribute RowsAttribute) else Nothing)
  , kAXVisibleRowsAttribute_ >>= \str -> cfEqual str attr >>= \b ->
      pure (if b then Just (SomeAttribute VisibleRowsAttribute) else Nothing)
  , kAXSelectedRowsAttribute_ >>= \str -> cfEqual str attr >>= \b ->
      pure (if b then Just (SomeAttribute SelectedRowsAttribute) else Nothing)
  , kAXColumnsAttribute_ >>= \str -> cfEqual str attr >>= \b ->
      pure (if b then Just (SomeAttribute ColumnsAttribute) else Nothing)
  , kAXVisibleColumnsAttribute_ >>= \str -> cfEqual str attr >>= \b ->
      pure (if b then Just (SomeAttribute VisibleColumnsAttribute) else Nothing)
  , kAXSelectedColumnsAttribute_ >>= \str -> cfEqual str attr >>= \b ->
      pure (if b then Just (SomeAttribute SelectedColumnsAttribute) else Nothing)
  , kAXSortDirectionAttribute_ >>= \str -> cfEqual str attr >>= \b ->
      pure (if b then Just (SomeAttribute SortDirectionAttribute) else Nothing)
  , kAXColumnHeaderUIElementsAttribute_ >>= \str -> cfEqual str attr >>= \b ->
      pure (if b then Just (SomeAttribute ColumnHeaderUIElementsAttribute) else Nothing)
  , kAXIndexAttribute_ >>= \str -> cfEqual str attr >>= \b ->
      pure (if b then Just (SomeAttribute IndexAttribute) else Nothing)
  , kAXDisclosingAttribute_ >>= \str -> cfEqual str attr >>= \b ->
      pure (if b then Just (SomeAttribute DisclosingAttribute) else Nothing)
  , kAXDisclosedRowsAttribute_ >>= \str -> cfEqual str attr >>= \b ->
      pure (if b then Just (SomeAttribute DisclosedRowsAttribute) else Nothing)
  , kAXDisclosedByRowAttribute_ >>= \str -> cfEqual str attr >>= \b ->
      pure (if b then Just (SomeAttribute DisclosedByRowAttribute) else Nothing)
  , kAXMatteHoleAttribute_ >>= \str -> cfEqual str attr >>= \b ->
      pure (if b then Just (SomeAttribute MatteHoleAttribute) else Nothing)
  , kAXMatteContentUIElementAttribute_ >>= \str -> cfEqual str attr >>= \b ->
      pure (if b then Just (SomeAttribute MatteContentUIElementAttribute) else Nothing)
  , kAXIsApplicationRunningAttribute_ >>= \str -> cfEqual str attr >>= \b ->
      pure (if b then Just (SomeAttribute IsApplicationRunningAttribute) else Nothing)
  , kAXFocusedApplicationAttribute_ >>= \str -> cfEqual str attr >>= \b ->
      pure (if b then Just (SomeAttribute FocusedApplicationAttribute) else Nothing)
  , kAXInsertionPointLineNumberAttribute_ >>= \str -> cfEqual str attr >>= \b ->
      pure (if b then Just (SomeAttribute InsertionPointLineNumberAttribute) else Nothing)
  ]

attrGetterSetter :: Attribute a -> (a -> IO CFTypeRef, CFTypeRef -> IO a)
attrGetterSetter = \case
  RoleAttribute ->
    (fmap asCFType . toRoleCFStringRef, fromRoleCFStringRef . castPtr)
  SubroleAttribute ->
    (pure . asCFType . toSubroleString, fromSubroleString . castPtr)
  RoleDescriptionAttribute -> cfObj
  HelpAttribute -> cfObj
  TitleAttribute -> cfObjSubty
  ValueAttribute -> cfObj
  MinValueAttribute -> cfObj
  MaxValueAttribute -> cfObj
  ValueIncrementAttribute -> cfObj
  AllowedValuesAttribute -> cfObj
  EnabledAttribute -> cfObj
  FocusedAttribute -> cfBool
  ParentAttribute -> cfObjSubty
  ChildrenAttribute -> cfObj
  SelectedChildrenAttribute -> cfObj
  VisibleChildrenAttribute -> cfObj
  WindowAttribute -> cfObj
  PositionAttribute ->
    ( join . fmap (flip withCFPtr pure) . createCFPoint
    , fmap (fromMaybe (error "failed to cast position attribute")) .
        axValueGetPoint . castPtr)
  TopLevelUIElementAttribute -> cfObjSubty
  SizeAttribute ->
    ( join . fmap (flip withCFPtr pure) . createCFSize
    , fmap (fromMaybe (error "failed to cast size attribute")) .
        axValueGetSize . castPtr)
  OrientationAttribute -> cfObj
  DescriptionAttribute -> cfObj
  SelectedTextAttribute -> cfObj
  SelectedTextRangeAttribute -> cfObj
  VisibleCharacterRangeAttribute -> cfObj
  NumberOfCharactersAttribute -> cfObj
  SharedTextUIElementsAttribute -> cfObj
  SharedCharacterRangeAttribute -> cfObj
  MainAttribute -> cfBool
  MinimizedAttribute -> cfBool
  CloseButtonAttribute -> cfObjSubty
  ZoomButtonAttribute -> cfObjSubty
  MinimizeButtonAttribute -> cfObjSubty
  ToolbarButtonAttribute -> cfObjSubty
  GrowAreaAttribute -> cfObj
  ProxyAttribute -> cfObj
  ModalAttribute -> cfObj
  DefaultButtonAttribute -> cfObjSubty
  CancelButtonAttribute -> cfObjSubty
  MenuItemCmdCharAttribute -> cfObj
  MenuItemCmdVirtualKeyAttribute -> cfObj
  MenuItemCmdGlyphAttribute -> cfObj
  MenuItemCmdModifiersAttribute -> cfObj
  MenuItemMarkCharAttribute -> cfObj
  MenuItemPrimaryUIElementAttribute -> cfObj
  MenuBarAttribute -> cfObj
  WindowsAttribute -> bimap (. getArray) (fmap Array .) cfObjSubty
  FrontmostAttribute -> cfObj
  HiddenAttribute -> cfObj
  MainWindowAttribute -> cfObj
  FocusedWindowAttribute -> cfObjSubty
  HeaderAttribute -> cfObj
  EditedAttribute -> cfObj
  TitleUIElementAttribute -> cfObj
  ValueWrapsAttribute -> cfObj
  TabsAttribute -> cfObj
  HorizontalScrollBarAttribute -> cfObj
  VerticalScrollBarAttribute -> cfObj
  OverflowButtonAttribute -> cfObj
  FilenameAttribute -> cfObj
  ExpandedAttribute -> cfObj
  SelectedAttribute -> cfObj
  SplittersAttribute -> cfObj
  NextContentsAttribute -> cfObj
  PreviousContentsAttribute -> cfObj
  DocumentAttribute -> cfObj
  IncrementButtonAttribute -> cfObj
  DecrementButtonAttribute -> cfObj
  ContentsAttribute -> cfObj
  IncrementorAttribute -> cfObj
  HourFieldAttribute -> cfObj
  MinuteFieldAttribute -> cfObj
  SecondFieldAttribute -> cfObj
  AMPMFieldAttribute -> cfObj
  DayFieldAttribute -> cfObj
  MonthFieldAttribute -> cfObj
  YearFieldAttribute -> cfObj
  ColumnTitleAttribute -> cfObj
  URLAttribute -> cfObj
  LabelUIElementsAttribute -> cfObj
  LabelValueAttribute -> cfObj
  ShownMenuUIElementAttribute -> cfObj
  ServesAsTitleForUIElementsAttribute -> cfObj
  LinkedUIElementsAttribute -> cfObj
  RowsAttribute -> cfObj
  VisibleRowsAttribute -> cfObj
  SelectedRowsAttribute -> cfObj
  ColumnsAttribute -> cfObj
  VisibleColumnsAttribute -> cfObj
  SelectedColumnsAttribute -> cfObj
  SortDirectionAttribute -> cfObj
  ColumnHeaderUIElementsAttribute -> cfObj
  IndexAttribute -> cfObj
  DisclosingAttribute -> cfObj
  DisclosedRowsAttribute -> cfObj
  DisclosedByRowAttribute -> cfObj
  MatteHoleAttribute -> cfObj
  MatteContentUIElementAttribute -> cfObj
  IsApplicationRunningAttribute -> cfObj
  FocusedApplicationAttribute -> cfObj
  InsertionPointLineNumberAttribute -> cfObj
  where
    cfObj :: (CFObject -> IO CFTypeRef, CFTypeRef -> IO CFObject)
    cfObj = (flip withCFPtr pure, retainManageCFObj)
    cfObjSubty :: CFClass a => (Object a -> IO CFTypeRef, CFTypeRef -> IO (Object a))
    cfObjSubty = (flip withCFPtr (pure . asCFType), retainManageCFObj . castPtr)
    cfBool = (fmap asCFType . boolToRef, refToBool . castPtr)
