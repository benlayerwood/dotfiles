module WindowHooks where

import XMonad.ManageHook
import XMonad.Util.NamedScratchpad
import qualified XMonad.StackSet as W
import Scratchpad (myScratchPads)

myManageHook = composeAll
    [ className =? "Gnome-calculator" --> doFloat
    , resource  =? "desktop_window"   --> doFloat
    , resource  =? "blueman-applet"   --> doF (W.shift "b")
    , resource  =? "Blueman-applet"   --> doF (W.shift "b")
    , resource  =? "pluma"            --> doFloat
    , className =? "VirtualBoxVM"     --> doFloat
    , className =? "zoom"             --> doFloat
    , className =? "zoom "            --> doFloat
    , resource  =? "Grid"             --> doFloat
    , resource  =? "JabRef"           --> doFloat
    , resource  =? "synapse"          --> doFloat
    , resource  =? "albert"           --> doFloat
    , resource  =? "anki"             --> doFloat
    , resource  =? "Anki"             --> doFloat
    , stringProperty "_NET_WM_NAME"    =? "Emulator"    --> doFloat
    , stringProperty "WM_NAME"         =? "Open Folder" --> doFloat
    , stringProperty "WM_NAME(STRING)" =? "Grid"        --> doFloat
    , namedScratchpadManageHook myScratchPads
    ]
