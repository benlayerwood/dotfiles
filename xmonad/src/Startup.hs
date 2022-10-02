module Startup where
import XMonad.Util.SpawnOnce
import XMonad.Util.Cursor
import XMonad.Hooks.SetWMName (setWMName)
import XMonad.Util.Run (spawnPipe)

myStartupHook = do
        setWMName "LG3D"
        spawnOnce "/bin/bash ~/.xmonad/polybar.sh"
        spawnOnce "~/.scripts/syncUni.sh"
        spawnOnce "/bin/bash ~/.fehbg"
        spawnOnce "/bin/bash ~/.xmonad/xrandrscript.sh"
        setDefaultCursor xC_arrow
        spawnOnce "bluetoothctl power on"
        spawnOnce "xscreensaver --no-splash"
        spawnOnce "blueman-applet"
        spawnOnce "nm-applet"
        spawnOnce "ayatana-webmail"
        spawnOnce "xscreensaver --no-splash"
