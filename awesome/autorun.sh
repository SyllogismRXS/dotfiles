#!/usr/bin/env bash

function run {
  if ! pgrep -f $1 ;
  then
    $@&
  fi
}

run autorandr --load desktop

run nm-applet

run volumeicon

#run autokey-gtk

#run blueman-applet
# xfce4-power-manager
# volumeicon

#LD_LIBRARY_PATH=/home/syllogismrxs/apps/Discord run /home/syllogismrxs/apps/Discord/Discord
run discord

#run slack

run xscreensaver -no-splash
