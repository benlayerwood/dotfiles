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
import Data.Monoid
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
import System.Exit (exitSuccess)

windowCount :: X (Maybe String)
windowCount = gets $ Just . show . length . W.integrate' . W.stack . W.workspace . W.current . windowset
myModMask       = mod4Mask

myNormalBorderColor  = "#242424"
myThemeColor = "#46dbf1"
--myThemeColor = "#ffd34a"

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
    , ((modm .|. shiftMask, xK_z        ), spawn "xmonad --recompile; xmonad --restart")
    , ((modm,               xK_c        ), spawn "gnome-calendar")
    , ((modm,               xK_b        ), spawn "firefox")
    , ((modm,               xK_n        ), spawn "nemo")
    , ((modm,               xK_m        ), spawn "emacs")
    , ((modm,               xK_space    ), spawn "rofi -columns 2 -show-icons -modi drun -show drun")
    , ((modm .|. shiftMask, xK_q        ), kill)
--  , ((modm .|. shiftMask, xK_t        ), scratchpadSpawnActionCustom "alacritty --config-file $HOME/.config/alacritty/alacritty-scratchpad.yml")
    , ((modm .|. shiftMask, xK_t        ), namedScratchpadAction myScratchPads "terminal")
    , ((modm,               xK_p        ), sendMessage NextLayout)
    , ((modm .|. shiftMask, xK_space    ), spawn "synapse")
--  , ((modm .|. shiftMask, xK_space ), setLayout $ XMonad.layoutHook conf)
    , ((modm,               xK_Tab      ), windows W.focusDown)
    , ((modm .|. shiftMask, xK_Tab      ), windows W.focusUp)
    , ((modm .|. shiftMask, xK_b        ), sequence_ [withFocused toggleBorder, withFocused $ windows . W.sink])
    , ((modm,               xK_t        ), withFocused $ windows . W.sink)
    , ((modm,               xK_j        ), windows W.focusDown)
    , ((modm,               xK_Page_Up  ), changeWorkspace (-1))
    , ((modm,               xK_Page_Down), changeWorkspace 1 )
    , ((modm .|. shiftMask, xK_Return   ), windows W.swapMaster)
    , ((modm .|. shiftMask, xK_j        ), windows W.swapDown)
    , ((modm .|. shiftMask, xK_k        ), windows W.swapUp    )
    , ((modm .|. shiftMask, xK_x        ), confirmPrompt myXPConfig "exit" $ io (exitWith ExitSuccess))
    , ((noModMask, xF86XK_MonBrightnessUp  ), spawn "xbacklight -inc 10")
    , ((noModMask, xF86XK_MonBrightnessDown), spawn "xbacklight -dec 10")
    , ((noModMask, xF86XK_AudioLowerVolume ), spawn "amixer set Master 7%- unmute")
    , ((noModMask, xF86XK_AudioRaiseVolume ), spawn "amixer set Master 7%+ unmute")
    , ((noModMask, xF86XK_AudioMicMute     ), spawn "amixer set Capture toggle")
    , ((noModMask, xF86XK_PowerOff         ), confirmPrompt myXPConfig "exit" $ io exitSuccess)
    , ((modm,               xK_h     ), sendMessage Shrink)
    , ((modm,               xK_l     ), sendMessage Expand)
    , ((modm,               xK_plus  ), sendMessage ToggleStruts)
    , ((modm,               xK_d     ), withFocused (keysResizeWindow (50,0) (0,0)))
    , ((modm,               xK_s     ), withFocused (keysResizeWindow (0,50) (0,0)))
    , ((modm .|. shiftMask, xK_d     ), withFocused (keysResizeWindow (-50,0)(0,0)))
    , ((modm .|. shiftMask, xK_s     ), withFocused (keysResizeWindow (0,-50) (0,0)))
--    , ((modm .|. shiftMask, xK_t     ), withFocused $ windows . W.float)
    , ((modm              , xK_comma ), sendMessage (IncMasterN 1))
    , ((modm              , xK_period), sendMessage (IncMasterN (-1)))
    , ((modm,                xK_f    ), setScreenWindowSpacing 0)
    , ((modm .|. shiftMask, xK_f     ), setScreenWindowSpacing 8)
    , ((modm .|. shiftMask, xK_l     ), spawn "xscreensaver-command -lock")
    , ((modm .|. shiftMask , xK_Up   ), spawn "amixer set Master 7%+ unmute")
    , ((modm .|. shiftMask , xK_Down ), spawn "amixer set Master 7%- unmute")
    , ((modm,               xK_g     ), goToSelected def)
    ]
    ++
    -- mod-[1..9], Switch to workspace N
    [((m .|. modm, k), windows $ f i)
        | (i, k) <- zip ["1","2","3","4","5","6","7","8","9","ext"] [xK_1 .. xK_9]
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
-- Window Decoration Theme
data SideDecoration a = SideDecoration Direction2D
  deriving (Show, Read)

instance Eq a => DecorationStyle SideDecoration a where

  shrink b (Rectangle _ _ dw dh) (Rectangle x y w h)
    | SideDecoration U <- b = Rectangle x (y + fi dh) w (h - dh)
    | SideDecoration R <- b = Rectangle x y (w - dw) h
    | SideDecoration D <- b = Rectangle x y w (h - dh)
    | SideDecoration L <- b = Rectangle (x + fi dw) y (w - dw) h

  pureDecoration b dw dh _ st _ (win, Rectangle x y w h)
    | win `elem` W.integrate st && dw < w && dh < h = Just $ case b of
      SideDecoration U -> Rectangle x y w dh
      SideDecoration R -> Rectangle (x + fi (w - dw)) y dw h
      SideDecoration D -> Rectangle x (y + fi (h - dh)) w dh
      SideDecoration L -> Rectangle x y dw h
    | otherwise = Nothing

myDecoration :: Eq a => l a -> ModifiedLayout (Decoration SideDecoration DefaultShrinker) l a
myDecoration = decoration shrinkText myTheme (SideDecoration U)

myTheme :: Theme
myTheme = def
  { activeColor = myThemeColor
  , activeBorderColor = myThemeColor
  , activeTextColor = myThemeColor
  , inactiveBorderColor = "#242424"
  , inactiveTextColor = "#242424"
  , inactiveColor = "#242424"
  , decoWidth = 3
  , decoHeight = 3
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
    , resource  =? "blueman-applet"   --> doF (W.shift "ext")
    , resource  =? "Blueman-applet"   --> doF (W.shift "ext")
    , resource  =? "pluma"            --> doFloat
    , className =? "VirtualBoxVM"     --> doFloat
    , className =? "zoom"             --> doFloat
    , className =? "zoom "            --> doFloat
    , resource  =? "Grid"             --> doFloat
--    , resource  =? "evince"            --> doF (W.shift "2")
    , resource  =? "JabRef"           --> doFloat
    , resource  =? "synapse"          --> doFloat
    , resource  =? "albert"           --> doFloat
    , (stringProperty "_NET_WM_NAME")    =? "Emulator"    --> doFloat
    , (stringProperty "WM_NAME")         =? "Open Folder" --> doFloat
    , (stringProperty "WM_NAME(STRING)") =? "Grid" --> doFloat
--  , scratchpadManageHook (W.RationalRect 0.2 0.2 0.6 0.7)
    , namedScratchpadManageHook myScratchPads
    ]

------------------------------------------------------------------------
-- Status bars and logging

-- Perform an arbitrary action on each internal state change or X event.
-- See the 'XMonad.Hooks.DynamicLog' extension for examples.
--
myLogHook = return ()

------------------------------------------------------------------------
-- Startup hook
myStartupHook = do
                spawn "/bin/bash ~/.xmonad/xrandrscript.sh; ~/.fehbg"
                spawn "/bin/bash ~/.fehbg"
                spawn "bluetoothctl power on"
                setDefaultCursor xC_arrow
                spawn "bluetoothctl power on"
                spawn "xscreensaver --no-splash"
                spawn "trayer --monitor 1 --edge top --align right --margin 15 --widthtype request --padding 0 --SetDockType true --SetPartialStrut true --expand true --transparent true --alpha 0 --tint 0x212121 --height 30"
                spawn "blueman-applet"
                spawn "nm-applet"
                spawn "killall volumeicon;volumeicon"
                spawn "ayatana-webmail"
--              spawn "picom --config ~/.config/picom/picom.conf"

------------------------------------------------------------------------
-- Now run xmonad with all the defaults we set up.
main = do
        xmproc <- spawnPipe $ "xmobar -x 0 $HOME/.config/xmobar/xmobarrc.hs"
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
            logHook = workspaceHistoryHook <+> myLogHook <+> dynamicLogWithPP (namedScratchpadFilterOutWorkspacePP  xmobarPP)
                            { ppOutput = hPutStrLn xmproc
                            , ppCurrent = xmobarColor myThemeColor "#3a3a3a" . wrap " " " " -- Current workspace in xmobar
                            , ppVisible = xmobarColor "#ffffff" ""                -- Visible but not current workspace
                            , ppHidden = xmobarColor "#8e8e8e" "" -- Hidden workspaces in xmobar
                            , ppHiddenNoWindows = xmobarColor "#3a3a3a" ""        -- Hidden workspaces (no windows)
                            , ppTitle = xmobarColor "#b3afc2" "" . shorten 60     -- Title of active window in xmobar
                            , ppSep = xmobarColor myThemeColor "" "  |  "
                            , ppUrgent = xmobarColor "#C45500" "" . wrap "!" "!"  -- Urgent workspace
                            , ppExtras  = [windowCount]
                            , ppLayout = drop 16                         -- remove SideDecoration String
                            , ppOrder  = \(ws:l:t:ex) -> [ws,l]++ex++[t]
--                            , ppSort    = getSortByXineramaRule
                            },
            startupHook        = myStartupHook
    }

