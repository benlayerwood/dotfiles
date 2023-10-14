#!/run/current-system/sw/bin/bash
monitors=$(xrandr --listmonitors | grep Monitors | cut -d: -f2)
if [ $monitors = "3" ]; then
   autorandr three
   i3-msg "workspace 3, move workspace to output eDP-1"
   i3-msg "workspace 1, move workspace to output DP-1"
fi
