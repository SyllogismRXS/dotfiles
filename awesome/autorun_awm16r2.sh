#!/usr/bin/env bash

xinput set-prop "$(xinput list --name-only | grep -i touch)" "libinput Tapping Enabled" 1

touch ~/.running_awm16r2.txt
