module Theme where

import XMonad.Layout.Decoration
import Graphics.X11.ExtraTypes.XF86
import Graphics.X11.Types
import XMonad.Prompt
import Data.List (isPrefixOf)

mediumblue = "#0181d1"
greyblue = "#425e68"
lightgrey = "#acacac"
darkgrey = "#212121"
white = "#ffffff"
purple = "#a232be"
yellow = "#e7b606"

myThemeColor = mediumblue
mySecondaryColor  = lightgrey
myBackgroundColor = darkgrey

myTheme :: Theme
myTheme = def
  { activeColor = myThemeColor
  , activeBorderColor = myThemeColor
  , activeTextColor = myThemeColor
  , inactiveBorderColor = lightgrey
  , inactiveTextColor = lightgrey
  , inactiveColor = lightgrey
  , decoWidth = 3
  , decoHeight = 3
  }

-- XPConfig Theme
myFont = "xft:Source Sans Pro:size=15:weight=Semibold"
myMonoFont = "xft:Fira Code:pixelsize=13:antialias=t"

tabTheme :: Theme
tabTheme = def
  { activeColor = greyblue
  , activeBorderColor = myThemeColor
  , inactiveBorderColor = myThemeColor
  , activeTextColor = white
  , inactiveTextColor = myThemeColor
  , inactiveColor = myBackgroundColor
  , activeBorderWidth = 0
  , inactiveBorderWidth = 0
  , decoWidth = 0
  , decoHeight = 23
  , fontName = myFont
  }

myXPConfig :: XPConfig
myXPConfig =
   XPC {
          font                  = myMonoFont
        , bgColor               = mySecondaryColor
        , fgColor               = myThemeColor
        , bgHLight              = mySecondaryColor
        , fgHLight              = myThemeColor
        , borderColor           = myThemeColor
        , promptBorderWidth     = 2
        , promptKeymap          = defaultXPKeymap
        , completionKey         = (0,xK_Tab)
        , changeModeKey         = xK_grave
        , position              = CenteredAt 0.5 0.3
        , height                = 33
        , maxComplRows          = Nothing
        , maxComplColumns       = Nothing
        , historySize           = 256
        , historyFilter         = id
        , defaultText           = []
        , autoComplete          = Nothing
        , showCompletionOnTab   = False
        , alwaysHighlight       = False
        , defaultPrompter       = id
        , sorter                = const id
        , complCaseSensitivity  = CaseSensitive
        , searchPredicate       = isPrefixOf
     }
