#!/usr/bin/env bash

function run {
  if ! pgrep -f $1 ;
  then
    $@&
  fi
}

pactl set-default-sink alsa_output.usb-BEHRINGER_UMC404HD_192k-00.analog-surround-40

touch ~/running_syllo-gpu.txt
