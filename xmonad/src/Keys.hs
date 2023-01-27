module Keys where

import XMonad
import qualified Data.Map        as M
import qualified XMonad.StackSet as W
import XMonad.Actions.GridSelect
import XMonad.Actions.FloatKeys
import XMonad.Hooks.ManageDocks
import XMonad.Layout.Spacing
import XMonad.Actions.NoBorders
import XMonad.Util.Scratchpad
import XMonad.Util.NamedScratchpad
import XMonad.Prompt.Shell
import XMonad.Prompt.ConfirmPrompt
import XMonad.Actions.CycleWS
import XMonad.Util.WorkspaceCompare
import System.Exit (exitSuccess)
import Graphics.X11.ExtraTypes.XF86
import Theme
import Constants
import Scratchpad
import XMonad.Actions.SwapWorkspaces (swapWithCurrent, swapWorkspaces)
import Graphics.X11.ExtraTypes (xF86XK_Calculater)

-- KeyCodes defined in /usr/include/X11/keysymdef.h
-- and https://hackage.haskell.org/package/X11-1.10.2/docs/Graphics-X11-ExtraTypes-XF86.html
myKeys conf@XConfig {XMonad.modMask = modm} = M.fromList $
    [ ((modm,               xK_Return      ), spawn $ XMonad.terminal conf)
    , ((modm,               xK_c           ), spawn "/opt/Morgen/morgen")
    , ((modm,               xK_b           ), spawn "librewolf")
    , ((modm,               xK_n           ), spawn "nemo")
    , ((modm,               xK_m           ), spawn "emacs")
    , ((modm,               xK_o           ), spawn "~/.scripts/monitors.sh")
    , ((modm,               xK_p           ), sendMessage NextLayout)
    , ((modm,               xK_t           ), withFocused $ windows . W.sink)
    , ((modm,               xK_g           ), goToSelected def)
    , ((modm,               xK_j           ), windows W.focusDown)
    , ((modm,               xK_h           ), sendMessage Shrink)
    , ((modm,               xK_l           ), sendMessage Expand)
    , ((modm,               xK_d           ), withFocused (keysResizeWindow (50,0) (0,0)))
    , ((modm,               xK_s           ), withFocused (keysResizeWindow (0,50) (0,0)))
    , ((modm,               xK_Tab         ), windows W.focusDown)
    , ((modm,               xK_Page_Up     ), changeWorkspace (-1))
    , ((modm,               xK_Page_Down   ), changeWorkspace 1 )
    , ((modm,               xK_plus        ), sendMessage ToggleStruts)
    , ((modm,               xK_comma       ), sendMessage (IncMasterN 1))
    , ((modm,               xK_period      ), sendMessage (IncMasterN (-1)))
    , ((modm,               xK_space       ), spawn "rofi -columns 2 -show-icons -modi \"drun,window,ssh\" -show drun -terminal alacritty")
    , ((modm,               xK_f           ), setScreenWindowSpacing 0)]
    ++
    [ ((modm .|. shiftMask, xK_b           ), sequence_ [withFocused toggleBorder, withFocused $ windows . W.sink])
    , ((modm .|. shiftMask, xK_q           ), kill)
    , ((modm .|. shiftMask, xK_t           ), namedScratchpadAction myScratchPads "terminal")
    , ((modm .|. shiftMask, xK_z           ), spawn "~/Code/xmonadben/compile.sh; xmonad --restart")
    , ((modm .|. shiftMask, xK_j           ), windows W.swapDown)
    , ((modm .|. shiftMask, xK_k           ), windows W.swapUp    )
    , ((modm .|. shiftMask, xK_x           ), confirmPrompt myXPConfig "exit" $ io exitSuccess)
    , ((modm .|. shiftMask, xK_d           ), withFocused (keysResizeWindow (-50,0)(0,0)))
    , ((modm .|. shiftMask, xK_s           ), withFocused (keysResizeWindow (0,-50) (0,0)))
    , ((modm .|. shiftMask, xK_f           ), setScreenWindowSpacing 14)
    , ((modm .|. shiftMask, xK_l           ), spawn "xscreensaver-command -lock")
    , ((modm .|. shiftMask, xK_space       ), spawn "synapse")
    , ((modm .|. shiftMask, xK_Tab         ), windows W.focusUp)
    , ((modm .|. shiftMask, xK_Return      ), windows W.swapMaster)
    , ((modm .|. shiftMask, xK_Up          ), spawn "amixer set Master 7%+ unmute")
    , ((modm .|. shiftMask, xK_Down        ), spawn "amixer set Master 7%- unmute")]
    ++
    [ ((noModMask, xF86XK_MonBrightnessUp  ), spawn "xbacklight -inc 10")
    , ((noModMask, xF86XK_MonBrightnessDown), spawn "xbacklight -dec 10")
    , ((noModMask, xF86XK_AudioLowerVolume ), spawn "amixer set Master 7%- unmute")
    , ((noModMask, xF86XK_AudioRaiseVolume ), spawn "amixer set Master 7%+ unmute")
    , ((noModMask, xF86XK_AudioMute        ), spawn "amixer set Master toggle")
    , ((noModMask, xF86XK_Calculator       ), spawn "gnome-calculator")
    , ((noModMask, xF86XK_PowerOff         ), confirmPrompt myXPConfig "exit" $ spawn "sudo shutdown -h now")
    ]
    ++
    -- mod-[1..9], Switch to workspace N
    [((m .|. modm, k), windows $ f i)
        | (i, k) <- zip myWorkspaces [xK_1 .. xK_9]
        , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]
    ++
    -- switch to screen
    [((m .|. modm, key), screenWorkspace sc >>= flip whenJust (windows . f))
        | (key, sc) <- zip [xK_e, xK_r, xK_w] [0..]
        , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]
    ++
    -- swap or rename screen
    [((m .|. modm, k), windows $ f i)
        | (i, k) <- zip myWorkspaces numPadKeys
        , (f, m) <- [(W.greedyView, 0), (swapWithCurrent, shiftMask)]]

numPadKeys = [ xK_KP_End,  xK_KP_Down,  xK_KP_Page_Down -- 1, 2, 3
             , xK_KP_Left, xK_KP_Begin, xK_KP_Right     -- 4, 5, 6
             , xK_KP_Home, xK_KP_Up,    xK_KP_Page_Up   -- 7, 8, 9
             , xK_KP_Insert] -- 0

myMouseBindings XConfig {XMonad.modMask = modm} = M.fromList
    [ ((modm, button1), \w -> focus w >> mouseMoveWindow w
                                       >> windows W.shiftMaster)
    , ((modm, button2), \w -> focus w >> windows W.shiftMaster)
    , ((modm, button3), \w -> focus w >> mouseResizeWindow w
                                       >> windows W.shiftMaster)
    ]

changeWorkspace :: Int -> X ()
changeWorkspace d = workspaceBy d >>= windows . W.view

workspaceBy :: Int -> X WorkspaceId
workspaceBy = findWorkspace getSortByIndex Next anyWS

--swapAndRename :: Eq i => i -> W.StackSet i l a s sd -> W.StackSet i l a s sd
--swapAndRename t s = ws . W.greedyView
--  where ws = swapWorkspaces t (W.currentTag s) s
