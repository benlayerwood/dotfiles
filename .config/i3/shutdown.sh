#!/usr/bin/env bash

# dmenu theming
lines="-l 4"
string="\
Exit i3\n\
Sleep\n\
Shutdown\n\
Reboot"

command=(
"i3-msg exit"
"systemctl suspend"
"sudo shutdown -h now"
"reboot"
)

selected="$(echo -e $string | \
            rofi -dmenu -i -p "Power Command" -format "i" \
            $lines)";

if [[ ! -z $selected ]]; then
       ${command[$selected]};
fi

exit 0
