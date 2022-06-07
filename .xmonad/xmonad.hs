{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}
import XMonad
import XMonad.Util.Run
import XMonad.Config.Desktop
import XMonad.Actions.NoBorders
import XMonad.Hooks.ManageDocks
import XMonad.Layout.IndependentScreens
import XMonad.Layout.Fullscreen
import XMonad.Layout.Decoration
import XMonad.Layout.Tabbed
import XMonad.Layout.Renamed
import XMonad.Layout.Reflect
import XMonad.Layout.Spacing
import XMonad.Layout.ThreeColumns
import XMonad.Util.EZConfig
import XMonad.Util.Types
import XMonad.Util.WorkspaceCompare
import XMonad.Util.NamedScratchpad
import XMonad.Util.Scratchpad
import XMonad.Util.NamedWindows (getName)
import XMonad.Util.SpawnOnce
import Data.Monoid
import Data.List (sortBy)
import Data.Function (on)
import Data.Maybe (fromMaybe)
import System.Exit
import Control.Monad

import XMonad.Actions.PhysicalScreens
import XMonad.Actions.CycleWS
import XMonad.Actions.GridSelect
import XMonad.Actions.FloatKeys
import XMonad.Hooks.WorkspaceHistory
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.EwmhDesktops
import XMonad.Prompt
import XMonad.Prompt.Shell
import XMonad.Prompt.ConfirmPrompt
import Graphics.X11.ExtraTypes.XF86

import qualified XMonad.StackSet as W
import qualified Data.Map        as M

import XMonad.Util.Cursor
import Constants
import Theme
import System.Exit (exitSuccess)

windowCount :: X (Maybe String)
windowCount = gets $ Just . show . length . W.integrate' . W.stack . W.workspace . W.current . windowset
myModMask       = mod4Mask

myNormalBorderColor  = "#242424"

myBorderWidth = 2

myLayout = spacingWithEdge 8 $ reflectHoriz $ avoidStruts $ tiled ||| tabbed ||| Mirror tiled ||| three
  where
     tiled = Tall nmaster delta ratio
     tabbed = renamed [Replace "Tabs"] $ tabbedBottom shrinkText myTheme
     three = renamed [Replace "Three"] $ Mirror $ ThreeCol nmaster delta (1/3)
     -- The default number of windows in the master pane
     nmaster = 1
     -- Default proportion of screen occupied by master pane
     ratio   = 1/2
     -- Percent of screen to increment by when resizing panes
     delta   = 10/100

-- KeyCodes defined in /usr/include/X11/keysymdef.h
-- and https://hackage.haskell.org/package/X11-1.10.2/docs/Graphics-X11-ExtraTypes-XF86.html
myKeys conf@XConfig {XMonad.modMask = modm} = M.fromList $
    [ ((modm , xK_Return), spawn $ XMonad.terminal conf)
    , ((modm,               xK_c           ), spawn "gnome-calendar")
    , ((modm,               xK_b           ), spawn "firefox")
    , ((modm,               xK_n           ), spawn "nemo")
    , ((modm,               xK_m           ), spawn "emacs")
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
    , ((modm,               xK_space       ), spawn "rofi -columns 2 -show-icons -modi drun -show drun")
    , ((modm,               xK_ssharp      ), windows $ W.greedyView "b")
    , ((modm,               xK_f           ), setScreenWindowSpacing 0)
    , ((modm .|. shiftMask, xK_b           ), sequence_ [withFocused toggleBorder, withFocused $ windows . W.sink])
    , ((modm .|. shiftMask, xK_q           ), kill)
    , ((modm .|. shiftMask, xK_t           ), namedScratchpadAction myScratchPads "terminal")
    , ((modm .|. shiftMask, xK_z           ), spawn "xmonad --recompile; xmonad --restart")
    , ((modm .|. shiftMask, xK_j           ), windows W.swapDown)
    , ((modm .|. shiftMask, xK_k           ), windows W.swapUp    )
    , ((modm .|. shiftMask, xK_x           ), confirmPrompt myXPConfig "exit" $ io (exitWith ExitSuccess))
    , ((modm .|. shiftMask, xK_d           ), withFocused (keysResizeWindow (-50,0)(0,0)))
    , ((modm .|. shiftMask, xK_s           ), withFocused (keysResizeWindow (0,-50) (0,0)))
    , ((modm .|. shiftMask, xK_f           ), setScreenWindowSpacing 8)
    , ((modm .|. shiftMask, xK_l           ), spawn "betterlockscreen -l dim")
    , ((modm .|. shiftMask, xK_space       ), spawn "synapse")
    , ((modm .|. shiftMask, xK_Tab         ), windows W.focusUp)
    , ((modm .|. shiftMask, xK_Return      ), windows W.swapMaster)
    , ((modm .|. shiftMask, xK_Up          ), spawn "amixer set Master 7%+ unmute")
    , ((modm .|. shiftMask, xK_Down        ), spawn "amixer set Master 7%- unmute")
    , ((noModMask, xF86XK_MonBrightnessUp  ), spawn "xbacklight -inc 10")
    , ((noModMask, xF86XK_MonBrightnessDown), spawn "xbacklight -dec 10")
    , ((noModMask, xF86XK_AudioLowerVolume ), spawn "amixer set Master 7%- unmute")
    , ((noModMask, xF86XK_AudioRaiseVolume ), spawn "amixer set Master 7%+ unmute")
    , ((noModMask, xF86XK_AudioMicMute     ), spawn "amixer set Capture toggle")
    , ((noModMask, xF86XK_PowerOff         ), confirmPrompt myXPConfig "exit" $ io exitSuccess)
    ]
    ++
    -- mod-[1..9], Switch to workspace N
    [((m .|. modm, k), windows $ f i)
        | (i, k) <- zip ["1","2","3","4","5","6","7","8","9","b"] [xK_1 .. xK_9]
        , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]
    ++
    -- switch to screen
    [((m .|. modm, key), screenWorkspace sc >>= flip whenJust (windows . f))
        | (key, sc) <- zip [xK_e, xK_r, xK_w] [0..]
        , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]
    ++
    -- swap screen window
    [((m .|. modm, k), windows $ f i)
        | (i, k) <- zip myWorkspaces numPadKeys
        , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]

numPadKeys = [ xK_KP_End,  xK_KP_Down,  xK_KP_Page_Down -- 1, 2, 3
             , xK_KP_Left, xK_KP_Begin, xK_KP_Right     -- 4, 5, 6
             , xK_KP_Home, xK_KP_Up,    xK_KP_Page_Up   -- 7, 8, 9
             , xK_KP_Insert] -- 0

changeWorkspace :: Int -> X ()
changeWorkspace d = workspaceBy d >>= windows . W.view

workspaceBy :: Int -> X (WorkspaceId)
workspaceBy = findWorkspace getSortByIndex Next AnyWS

------------------------------------------------------------------------
-- Mouse Bindings
myMouseBindings XConfig {XMonad.modMask = modm} = M.fromList
    [ ((modm, button1), \w -> focus w >> mouseMoveWindow w
                                       >> windows W.shiftMaster)
    , ((modm, button2), \w -> focus w >> windows W.shiftMaster)
    , ((modm, button3), \w -> focus w >> mouseResizeWindow w
                                       >> windows W.shiftMaster)
    ]

------------------------------------------------------------------------
-- Scratchpads
myScratchPads :: [NamedScratchpad]
myScratchPads = [ NS "terminal" spawnTerm findTerm manageTerm]
   where
    spawnTerm  = "alacritty --config-file $HOME/.config/alacritty/alacritty-scratchpad.yml --command /bin/bash"
    findTerm   = resource =? "scratchpad"
    manageTerm = customFloating $ W.RationalRect 0.2 0.2 0.6 0.7

------------------------------------------------------------------------
-- XPConfig Theme
myFont = "xft:Source Sans Pro:size=15:weight=Semibold"

myXPConfig :: XPConfig
myXPConfig =
   XPC {
          font                  = myFont
        , bgColor               = myNormalBorderColor
        , fgColor               = myThemeColor
        , bgHLight              = myNormalBorderColor
        , fgHLight              = myThemeColor
        , borderColor           = border def
        , promptBorderWidth     = 2
        , promptKeymap          = defaultXPKeymap
        , completionKey         = (0,xK_Tab)
        , changeModeKey         = xK_grave
        , position              = CenteredAt 0.95 0.5
        , height                = 33
        , maxComplRows          = Nothing
        , historySize           = 256
        , historyFilter         = id
        , defaultText           = []
        , autoComplete          = Nothing
        , showCompletionOnTab   = False
        , alwaysHighlight       = False
        , defaultPrompter       = id
        , sorter                = const id
     }

------------------------------------------------------------------------
-- Window rules:

-- Execute arbitrary actions and WindowSet manipulations when managing
-- a new window. You can use this to, for example, always float a
-- particular program, or have a client always appear on a particular
-- workspace.
--
-- To find the property name associated with a program, use
-- > xprop | grep WM_CLASS
-- and click on the client you're interested in.
--
-- To match on the WM_NAME, you can use 'title' in the same way that
-- 'className' and 'resource' are used below.
--
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
    , (stringProperty "_NET_WM_NAME")    =? "Emulator"    --> doFloat
    , (stringProperty "WM_NAME")         =? "Open Folder" --> doFloat
    , (stringProperty "WM_NAME(STRING)") =? "Grid" --> doFloat
    , namedScratchpadManageHook myScratchPads
    ]

------------------------------------------------------------------------
-- Status bars and logging

-- Perform an arbitrary action on each internal state change or X event.
-- See the 'XMonad.Hooks.DynamicLog' extension for examples.
--
myLogHook = do
  winset <- gets windowset
  title <- maybe (return "") (fmap show . getName) . W.peek $ winset
  count <- gets $ Just . show . length . W.integrate' . W.stack . W.workspace . W.current . windowset
  let layout = description . W.layout . W.workspace . W.current $ winset
  io $ appendFile "/tmp/.xmonad-window-count" (fromMaybe "" count ++ "\n")
  io $ appendFile "/tmp/.xmonad-title-log" ((shorten 50 title) ++ "\n")
  -- TODO: layout output not working yet
  io $ appendFile "/tmp/.xmonad-layout-log" (layout ++ "\n")

------------------------------------------------------------------------
-- Startup hook
myStartupHook = do
                spawnOnce "betterlockscreen --wall --off 300 dim -u \'/home/ben/Pictures/Wallpapers/selection/\'"
                spawnOnce "/bin/bash ~/.xmonad/xrandrscript.sh; /bin/bash ~/.fehwp"
                setDefaultCursor xC_arrow
                spawnOnce "bluetoothctl power on"
                spawnOnce "xscreensaver --no-splash"
                spawnOnce "blueman-applet"
                spawnOnce "nm-applet"
                spawnOnce "ayatana-webmail"
                spawnOnce "polybar"
                spawn "/bin/bash ~/.fehwp"
--              spawn "picom --config ~/.config/picom/picom.conf"

------------------------------------------------------------------------
-- Now run xmonad with all the defaults we set up.
main = do
        forM_ [".xmonad-workspace-log", ".xmonad-title-log", ".xmonad-window-count"] $ \file -> do
            safeSpawn "mkfifo" ["/tmp/" ++ file]
        xmonad $ docks $ ewmh $ def {
            manageHook         = myManageHook,
            terminal           = myTerminal,
            focusFollowsMouse  = myFocusFollowsMouse,
            clickJustFocuses   = myClickJustFocuses,
            borderWidth        = myBorderWidth,
            modMask            = myModMask,
            workspaces         = myWorkspaces,
            normalBorderColor  = myNormalBorderColor,
            focusedBorderColor = myThemeColor,
            keys               = myKeys,
            mouseBindings      = myMouseBindings,
            layoutHook         = myLayout,
            handleEventHook    = handleEventHook def <+> XMonad.Hooks.EwmhDesktops.fullscreenEventHook,
            logHook            = myLogHook,
            startupHook        = myStartupHook
    }

