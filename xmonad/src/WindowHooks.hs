module WindowHooks where

import XMonad.ManageHook
import XMonad.Util.NamedScratchpad
import qualified XMonad.StackSet as W
import Scratchpad (myScratchPads)

myManageHook = composeAll
    [ className =? "Gnome-calculator" --> doFloat
    , resource  =? "desktop_window"   --> doFloat
    , resource  =? "pluma"            --> doFloat
    , className =? "VirtualBoxVM"     --> doFloat
    , className =? "zoom"             --> doFloat
    , className =? "zoom "            --> doFloat
    , resource  =? "Grid"             --> doFloat
    , resource  =? "JabRef"           --> doFloat
    , resource  =? "synapse"          --> doFloat
    , resource  =? "albert"           --> doFloat
    , stringProperty "_NET_WM_NAME"    =? "Emulator"    --> doFloat
    , stringProperty "WM_NAME"         =? "Open Folder" --> doFloat
    , stringProperty "WM_NAME(STRING)" =? "Grid"        --> doFloat
    , namedScratchpadManageHook myScratchPads
    ]
