module MacSdk.Framework.CoreGraphics.Space where

import Control.Monad
import Foreign
import Foreign.C.Types (CInt(..), CUInt(..))
import MacSdk.Framework.CoreGraphics.Connection
import MacSdk.Framework.CoreFoundation
import MacSdk.Framework.Types
import Prelude hiding (String)
import Control.Arrow ((>>>))

type SpaceID = Word32
type CGSSpaceType = CInt

data SpaceType = UserSpace | SystemSpace | FullscreenSpace deriving (Eq, Show)

toSpaceType :: CGSSpaceType -> SpaceType
toSpaceType st
  | st == cgSSpaceTypeUser = UserSpace
  | st == cgSSpaceTypeFullscreen = FullscreenSpace
  | st == cgSSpaceTypeSystem = SystemSpace
  | otherwise = error "unknown CGSSpaceType code"

fromSpaceType :: SpaceType -> CGSSpaceType
fromSpaceType UserSpace = cgSSpaceTypeUser
fromSpaceType SystemSpace = cgSSpaceTypeSystem
fromSpaceType FullscreenSpace = cgSSpaceTypeFullscreen

cgSSpaceTypeUser, cgSSpaceTypeFullscreen, cgSSpaceTypeSystem :: CGSSpaceType
cgSSpaceTypeUser = 0
cgSSpaceTypeFullscreen = 4
cgSSpaceTypeSystem = 2

foreign import ccall unsafe "CGSManagedDisplayGetCurrentSpace"
  cgSManagedDisplayGetCurrentSpace
  :: ConnectionID -> CFStringRef -> IO SpaceID

currentSpace :: ConnectionID -> CFString -> IO SpaceID
currentSpace cid str = withCFPtr str $ cgSManagedDisplayGetCurrentSpace cid

currentSpace' :: CFString -> IO SpaceID
currentSpace' uuid = defaultConnection >>= \conn -> currentSpace conn uuid

foreign import ccall unsafe "CGSSpaceGetType"
  cgSSpaceGetType :: ConnectionID -> SpaceID -> IO CGSSpaceType

spaceType :: ConnectionID -> SpaceID -> IO SpaceType
spaceType cid sid = fmap toSpaceType $ cgSSpaceGetType cid sid

spaceType' :: SpaceID -> IO SpaceType
spaceType' sid = defaultConnection >>= \conn -> spaceType conn sid

type CGSSpaceSelector = CInt

data SpaceSelector = SpaceCurrent | SpaceOther | SpaceAll deriving (Eq, Show)

fromSelector :: SpaceSelector -> CGSSpaceSelector
fromSelector SpaceCurrent = 5
fromSelector SpaceOther = 6
fromSelector SpaceAll = 7

foreign import ccall unsafe "CGSCopySpacesForWindows"
  cgSCopySpacesForWindows
  :: ConnectionID -> CGSSpaceSelector -> CFArrayRef -> IO CFArrayRef

spacesForWindows
  :: ConnectionID -> SpaceSelector -> Array Number -> IO (Array Number)
spacesForWindows conn sel arr = withCFPtr (getArray arr) $ \parr ->
  cgSCopySpacesForWindows conn (fromSelector sel) parr >>=
    (manageCFObj >>> fmap Array)

foreign import ccall unsafe "CGSMoveWindowsToManagedSpace"
  cgsMoveWindowsToManagedSpace
  :: ConnectionID -> CFArrayRef -> ForeignSpaceID -> IO ()

moveWindowsToSpace :: ConnectionID -> Array Number -> SpaceID -> IO ()
moveWindowsToSpace cid arr sid = withCFPtr (getArray arr) $ \parr ->
  cgsMoveWindowsToManagedSpace cid parr (fromIntegral sid)

foreign import ccall "CGSCopyManagedDisplaySpaces"
  cgsCopyManagedDisplaySpaces :: ConnectionID -> IO CFArrayRef

managedDisplaySpaces :: ConnectionID -> IO (Array Dictionary)
managedDisplaySpaces = cgsCopyManagedDisplaySpaces >=> manageCFObj >>> fmap Array

--------------------------------------------------------------------------------

type ForeignSpaceID = CUInt

-- foreign import ccall "init_workspace" startWorkspaceEvents :: IO ()

data SpaceEvent
  = SEDisplayChanged
  | SESpaceChanged
  | SEAppActivated
  | SEAppDeactivated
  | SEAppHidden
  | SEAppVisible
  deriving (Eq, Show)

seFromInt :: Integral a => a -> SpaceEvent
seFromInt e | e == 0 = SEDisplayChanged
seFromInt e | e == 1 = SESpaceChanged
seFromInt e | e == 2 = SEAppActivated
seFromInt e | e == 3 = SEAppDeactivated
seFromInt e | e == 4 = SEAppHidden
seFromInt e | e == 5 = SEAppVisible
seFromInt _ = error "unknown SpaceEvent code"

type ForeignWorkspaceCallback = CUInt -> PID -> IO ()
type WorkspaceCallback = SpaceEvent -> PID -> IO ()

toForeignWCallb :: WorkspaceCallback -> ForeignWorkspaceCallback
toForeignWCallb f e = f (seFromInt e)

foreign import ccall "wrapper" wrap_workspace_callback
  :: ForeignWorkspaceCallback -> IO (FunPtr ForeignWorkspaceCallback)

-- foreign import ccall unsafe
--   set_workspace_callback :: FunPtr ForeignWorkspaceCallback -> IO ()

data WorkspaceWatcher_
newtype WorkspaceWatcher = WorkspaceWatcher (Ptr WorkspaceWatcher_)
foreign import ccall unsafe new_workspace_watcher
  :: FunPtr ForeignWorkspaceCallback -> IO (Ptr WorkspaceWatcher_)
foreign import ccall unsafe stop_workspace_watcher
  :: Ptr WorkspaceWatcher_ -> IO ()

newWorkspaceWatcher :: WorkspaceCallback -> IO WorkspaceWatcher
newWorkspaceWatcher f = do
  callb <- wrap_workspace_callback (toForeignWCallb f)
  WorkspaceWatcher <$> new_workspace_watcher callb

stopWorkspaceWatcher :: WorkspaceWatcher -> IO ()
stopWorkspaceWatcher (WorkspaceWatcher p) = stop_workspace_watcher p

-- setWorkspaceCallback :: WorkspaceCallback -> IO ()
-- setWorkspaceCallback f =
--   wrap_workspace_callback (toForeignWCallb f) >>= set_workspace_callback

-- foreign import ccall count_desktops_for_display :: CFDictionaryRef -> IO Word32

-- count_desktops_for_display' :: Dictionary -> IO Int
-- count_desktops_for_display' dict =
--   fmap fromIntegral $ withCFPtr dict count_desktops_for_display
