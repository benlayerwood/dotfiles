#!/usr/bin/env sh

focused_output=$(i3-msg -t get_workspaces | jq -r '.[] | select(.focused==true).output')

# Move windows from focused monitor to scratchpad
i3-msg "workspace scratchpad; move workspace to output $focused_output"

# Move windows from target monitor to original workspace
i3-msg "workspace back_and_forth; move workspace to output $focused_output"
