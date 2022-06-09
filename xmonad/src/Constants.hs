module Constants where
import Graphics.X11.Types
import Graphics.X11.Xlib

myTerminal      = "WINIT_X11_SCALE_FACTOR=1.66 alacritty"
myFocusFollowsMouse :: Bool
myFocusFollowsMouse = True
myClickJustFocuses :: Bool
myClickJustFocuses = False
myWorkspaces :: [String] 
myWorkspaces    = ["1","2","3","4","5","6","7","8","9","b"]
myModMask       = mod4Mask
myBorderWidth :: Dimension
myBorderWidth = 2
