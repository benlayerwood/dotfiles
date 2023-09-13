module Main where

import XMonad
import XMonad.Util.Run
import XMonad.Config.Desktop
import XMonad.Util.EZConfig
import XMonad.Util.Types
import XMonad.Util.SpawnOnce
import XMonad.Prompt.ConfirmPrompt
import Data.Monoid
import Control.Monad

import XMonad.Layout.IndependentScreens
import XMonad.Layout.Fullscreen
import XMonad.Layout.Decoration
import XMonad.Layout.Renamed
import XMonad.Layout.Reflect
import XMonad.Layout.Spacing
import XMonad.Layout.ThreeColumns

import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.Rescreen

import qualified Data.Map        as M
import qualified XMonad.StackSet as W

import Constants
import Keys (myKeys, numPadKeys, changeWorkspace, myMouseBindings, workspaceBy)
import Startup (myStartupHook)
import Log (myLogHook)
import WindowHooks
import XMonad.Layout.Tabbed (tabbed)
import XMonad.Layout.Groups.Wmii (shrinkText)
import Theme
import XMonad.Layout.NoBorders (lessBorders, Ambiguity (OnlyFloat, OnlyScreenFloat))

mySpacing = 0
myLayout = lessBorders OnlyScreenFloat $ spacingWithEdge mySpacing $ reflectHoriz $ avoidStruts $ tabs ||| tiled ||| Mirror tiled
  where
     tiled = Tall nmaster delta ratio
     tabs = renamed [Replace "Tabs"] $ tabbed shrinkText tabTheme
     -- The default number of windows in the master pane
     nmaster = 1
     -- Default proportion of screen occupied by master pane
     ratio   = 1/2
     -- Percent of screen to increment by when resizing panes
     delta   = 10/100


------------------------------------------------------------------------
-- Now run xmonad with all the defaults we set up.
main = do
        xmonad $ docks . ewmhFullscreen . ewmh $ def {
            manageHook         = myManageHook,
            terminal           = myTerminal,
            focusFollowsMouse  = myFocusFollowsMouse,
            clickJustFocuses   = myClickJustFocuses,
            borderWidth        = myBorderWidth,
            modMask            = myModMask,
            workspaces         = myWorkspaces,
            normalBorderColor  = mySecondaryColor,
            focusedBorderColor = myThemeColor,
            keys               = myKeys,
            mouseBindings      = myMouseBindings,
            layoutHook         = myLayout,
            handleEventHook    = handleEventHook def,
            logHook            = myLogHook,
            startupHook        = myStartupHook
    }
