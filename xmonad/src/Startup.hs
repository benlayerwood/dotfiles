module Startup where
import XMonad.Util.SpawnOnce
import XMonad.Util.Cursor
import XMonad.Hooks.SetWMName (setWMName)

myStartupHook = do
        setWMName "LG3D"
        spawnOnce "/bin/bash ~/.fehbg"
        spawnOnce "/bin/bash ~/.xmonad/xrandrscript.sh"
        setDefaultCursor xC_arrow
        spawnOnce "bluetoothctl power on"
        spawnOnce "xscreensaver --no-splash"
        spawnOnce "blueman-applet"
        spawnOnce "nm-applet"
        spawnOnce "ayatana-webmail"
        spawnOnce "polybar"
        spawnOnce "xscreensaver --no-splash"
