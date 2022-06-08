module Theme where
import XMonad.Layout.Decoration

myThemeColor = "#6ec4dd"

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
