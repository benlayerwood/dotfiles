module Scratchpad where

import XMonad.Util.NamedScratchpad
import XMonad.ManageHook
import qualified XMonad.StackSet as W

myScratchPads :: [NamedScratchpad]
myScratchPads = [ NS "terminal" spawnTerm findTerm manageTerm]
   where
    spawnTerm  = "alacritty --config-file $HOME/.config/alacritty/alacritty-scratchpad.yml --command /bin/bash"
    findTerm   = resource =? "scratchpad"
    manageTerm = customFloating $ W.RationalRect 0.2 0.2 0.6 0.7
