module Log where

import XMonad
import qualified XMonad.StackSet as W
import XMonad.Util.NamedWindows (getName)
import Data.Maybe (fromMaybe)
import XMonad.Hooks.DynamicLog

myLogHook = do
  winset <- gets windowset
  title <- maybe (return "") (fmap show . getName) . W.peek $ winset
  count <- gets $ Just . show . length . W.integrate' . W.stack . W.workspace . W.current . windowset
  let layout = description . W.layout . W.workspace . W.current $ winset
  io $ appendFile "/tmp/.xmonad-window-count" (fromMaybe "" count ++ "\n")
  io $ appendFile "/tmp/.xmonad-title-log" (shorten 50 title ++ "\n")
  io $ appendFile "/tmp/.xmonad-layout-log" (drop (length "Spacing ReflectX ") layout ++ "\n")
