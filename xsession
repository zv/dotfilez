#!/bin/bash
# Load resources
# xrdb -merge .Xresources
# Set the background color
xsetroot -solid white 
 
# remap capslock
xmodmap /home/zv/.xmodmaprc &

## Fire up apps
xscreensaver -no-splash &
xhost local:
xmonad

autocutsel &
