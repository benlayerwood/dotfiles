#!/usr/bin/env bash

# dmenu theming
lines="-l 6"
string="\
1 Monitor\n\
2 Monitors: Mirror\n\
2 Monitors: External Only (HDMI)\n\
2 Monitors: Extend Right (HDMI)\n\
2 Monitors: Extend Left (HDMI)\n\
3 Monitors"

command=(
mobile
twomirror
twoexternal
twoextendright
twoextendleft
three
)

selected="$(echo -e $string | \
            rofi -dmenu -i -p "Select Screen Configuration" -format "i" \
            $lines)";

if [[ ! -z $selected ]]; then
        autorandr ${command[$selected]};
fi

exit 0
