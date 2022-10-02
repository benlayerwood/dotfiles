#!/bin/bash
# 3-Monitor Setup
xrandr --output DP1 --mode 1920x1080 --pos 0x0 --rotate right --left-of HDMI1 --output HDMI1 --primary --mode 1920x1080 --pos 1080x420 --rotate normal --left-of eDP1 --output HDMI2 --off --output VIRTUAL1 --off --output eDP1 --mode 1366x768 --pos 3000x624 --rotate normal

# 2-Monitor Setup
# xrandr --output eDP1 --mode 1366x768 --pos 0x156 --rotate normal --output DP1 --off --output HDMI1 --primary --mode 1920x1080 --pos 1366x0 --rotate normal --output HDMI2 --off --output VIRTUAL1 --off
