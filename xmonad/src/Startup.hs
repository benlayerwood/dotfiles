module Startup where

import XMonad.Actions.DynamicWorkspaceOrder
import XMonad.Util.SpawnOnce
import XMonad.Util.Cursor
import Constants
import XMonad.Hooks.SetWMName (setWMName)
import XMonad.Util.Run (spawnPipe)

myStartupHook = do
        setWMName "LG3D"
        setDefaultCursor xC_arrow
        spawnOnce "ayatana-webmail"
        spawnOnce "/bin/bash ~/.xmonad/polybar.sh"
        spawnOnce "nohup /opt/bwsync &> /dev/null &"
        spawnOnce "~/.scripts/syncUni.sh"
        spawnOnce "/bin/bash ~/.fehbg"
        spawnOnce "/bin/bash ~/.xmonad/xrandrscript.sh"
        spawnOnce "blueman-applet"
        spawnOnce "nm-applet"
