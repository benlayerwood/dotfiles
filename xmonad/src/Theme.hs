module Theme where

import XMonad.Layout.Decoration
import Graphics.X11.ExtraTypes.XF86
import Graphics.X11.Types
import XMonad.Prompt
import Data.List (isPrefixOf)

myThemeColor = "#0181d1"
--myThemeColor = "#ff0000"
mySecondaryColor  = "#acacac"
myBackgroundColor = "#212121"

myTheme :: Theme
myTheme = def
  { activeColor = myThemeColor
  , activeBorderColor = myThemeColor
  , activeTextColor = myThemeColor
  , inactiveBorderColor = "#acacac"
  , inactiveTextColor = "#acacac"
  , inactiveColor = "#acacac"
  , decoWidth = 3
  , decoHeight = 3
  }

-- XPConfig Theme
myFont = "xft:Source Sans Pro:size=15:weight=Semibold"
myMonoFont = "xft:Fira Code:size=8"

tabTheme = def
  { activeColor = "#425e68"
  , activeBorderColor = myThemeColor
  , inactiveBorderColor = myThemeColor
  , activeTextColor = "#ffffff"
  , inactiveTextColor = myThemeColor
  , inactiveColor = myBackgroundColor
  , activeBorderWidth = 0
  , inactiveBorderWidth = 0
  , decoWidth = 0
  , decoHeight = 23
  , fontName = myMonoFont
  }

myXPConfig :: XPConfig
myXPConfig =
   XPC {
          font                  = myFont
        , bgColor               = mySecondaryColor
        , fgColor               = myThemeColor
        , bgHLight              = mySecondaryColor
        , fgHLight              = myThemeColor
        , borderColor           = border def
        , promptBorderWidth     = 2
        , promptKeymap          = defaultXPKeymap
        , completionKey         = (0,xK_Tab)
        , changeModeKey         = xK_grave
        , position              = CenteredAt 0.95 0.5
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
