#!/bin/bash

stack build &> compile.log && rm ~/.xmonad/xmonad-x86_64-linux && \
    ln ~/Code/xmonadben/.stack-work/dist/x86_64-linux-tinfo6/Cabal-3.4.1.0/build/xmonadben-exe/xmonadben-exe ~/.xmonad/xmonad-x86_64-linux
