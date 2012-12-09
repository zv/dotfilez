#!/bin/bash
# Load resources
# xrdb -merge .Xresources
# Set the background color
xsetroot -solid black 
 
# Fire up apps
xscreensaver -no-splash &
xhost local:
xmonad
