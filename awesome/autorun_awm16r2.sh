#!/usr/bin/env bash

# Enable tap-to-click on trackpad
xinput set-prop "$(xinput list --name-only | grep -i touch)" "libinput Tapping Enabled" 1

# Lower resolution, default is 2560x1600
#xrandr -s 1920x1200

touch ~/.running_awm16r2.txt
