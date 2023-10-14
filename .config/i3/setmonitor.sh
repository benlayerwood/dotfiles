#!/bin/bash
set k=$(echo $DISPLAY | cut -d: -f2)
test 1 -eq 1 && xrandr --output HDMI1 --auto --left-of eDP1
exit
