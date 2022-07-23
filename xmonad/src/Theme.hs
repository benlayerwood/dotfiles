module Theme where

import XMonad.Layout.Decoration
import Graphics.X11.ExtraTypes.XF86
import Graphics.X11.Types
import XMonad.Prompt
import Data.List (isPrefixOf)

myThemeColor = "#0181d1"
--myThemeColor = "#ff0000"
myNormalBorderColor  = "#acacac"

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
